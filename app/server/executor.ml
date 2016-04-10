(*
 * Accretio is an API, a sandbox and a runtime for social playbooks
 *
 * Copyright (C) 2015 William Le Ferrand
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)


open Printf
open Lwt

open CalendarLib
open Eliom_content.Html5
open Eliom_content.Html5.D

open Ys_uid
open Ys_executor

open Api

module SetString = Set.Make(String)

(* the context factory -- there is a little bit a first class module magic
   so that pa_playbook can craft a context specific to each module *)

module type MODE_SPECIFICS = functor (Stage_specifics : STAGE_SPECIFICS) ->
sig

  val log_info : ('a, unit, string, unit) Pervasives.format4 -> 'a
  val log_warning : ?exn:exn -> ('a, unit, string, unit) Pervasives.format4 -> 'a
  val log_error :  ?exn:exn -> ('a, unit, string, unit) Pervasives.format4 -> 'a

  val message_member : member:uid -> ?attachments:Object_message.attachments -> ?data:(string * string) list -> subject:string -> content:Html5_types.div_content_fun elt list -> unit -> uid option Lwt.t
  val reply_to : message:uid -> ?original_stage:bool -> ?data:(string * string) list -> content:Html5_types.div_content_fun elt list -> unit -> uid option Lwt.t
  val forward_to_supervisor : message:uid -> ?data:(string * string) list -> subject:string -> content:Html5_types.div_content_fun elt list -> unit -> uid option Lwt.t

end

let reminder_label uid = sprintf "remindemail%d" uid

let check_missing_parameters society =
  lwt playbook, data = $society(society)->(playbook, data) in
  lwt parameters = $playbook(playbook)->parameters in
  let data =
    List.fold_left
      (fun acc (key, _) -> SetString.add key acc)
      SetString.empty
      data
  in
  let missing_parameters =
    List.fold_left
      (fun acc parameter ->
         if SetString.mem parameter.Object_playbook.key data then acc else parameter :: acc)
      []
      parameters
  in
  return missing_parameters


let context_factory mode society =

  lwt society_name, society_description, society_shortlink, society_supervisor = $society(society)->(name, description, shortlink, leader) in
  let direct_link = (Ys_config.get_string "url-prefix")^"/society/"^society_shortlink in

  let mode_specifics =
    match mode with
    | `Sandbox ->
      let module Mode_Specifics (Stage_Specifics: STAGE_SPECIFICS) =
      struct

        include Stage_Specifics

        let log_info = fun fmt ->
          let source = sprintf "context-info-%d" society in
          let timestamp = Ys_time.now () in
          Printf.ksprintf
            (fun message ->
               Lwt_log.ign_info_f "society %d: %s" society message ;
               ignore_result (Logs.insert source timestamp message)) fmt

        let log_warning = fun ?exn fmt ->
          let source = sprintf "context-warning-%d" society in
          let timestamp = Ys_time.now () in
          Printf.ksprintf
            (fun message ->
               Lwt_log.ign_warning_f ?exn "society %d: %s" society message ;
               ignore_result (Logs.insert source timestamp message)) fmt

        let log_error = fun ?exn fmt ->
          let source = sprintf "context-error-%d" society in
          let timestamp = Ys_time.now () in
          Printf.ksprintf
            (fun message ->
               Lwt_log.ign_error_f ?exn "society %d: %s" society message ;
               ignore_result (Logs.insert source timestamp message)) fmt

        let send_message ?(attachments=[]) ?(reference=None) ?(data=[]) member subject content =
          let origin = Object_message.Stage Stage_Specifics.stage in
          lwt uid =
            match_lwt Object_message.Store.create
                        ~society
                        ~origin
                        ~content
                        ~subject
                        ~attachments
                        ~destination:(Object_message.Member member)
                        ~reference:(Object_message.create_reference subject)
                        ~references:(match reference with Some reference -> [ reference ] | None -> [])
                        () with
            | `Object_already_exists (_, uid) -> return uid
            | `Object_created message -> return message.Object_message.uid
          in
          lwt reference = $message(uid)->reference in
          lwt _ =
            $society(society)<-data %% (fun store ->
                List.fold_left
                  (fun acc (key, value) ->
                     (reference^"-"^key, value) :: acc)
                  store
                  data)
          in
          lwt _ = $society(society)<-outbox += (`Message (Object_society.({ received_on = Ys_time.now (); read = false })), uid) in
          log_info "message attached from member %d to society %d" member society ;
          return (Some uid)

        let message_member ~member ?(attachments=[]) ?(data=[]) ~subject ~content () =
          let flat_content = ref "" in
          Printer.print_list (fun s -> flat_content := !flat_content ^ s) content ;
          send_message ~data ~attachments member subject !flat_content

        let reply_to ~message ?(original_stage=false) ?(data=[]) ~content () =
          lwt subject = $message(message)->subject in
          match_lwt $message(message)->(origin, destination) with
          | Object_message.Member member, _ | _, Object_message.Member member ->
            message_member ~member ~subject:subject ~data ~content ()
          | _ ->
            log_error "can't reply_to message %d" message ;
            return_none

        let forward_to_supervisor ~message ?(data=[]) ~subject ~content () =
          lwt original_content = $message(message)->raw in
          lwt original_origin, original_destination = $message(message)->(origin, destination) in
          let format_interlocutor =
            function
              Object_message.Stage stage ->
              return (Printf.sprintf "Stage %s" stage)
            | Object_message.Member member ->
              lwt name, preferred_email = $member(member)->(name, preferred_email) in
              return (Printf.sprintf "Member %s <%s>" name preferred_email)
            | Object_message.CatchAll ->
              return "CatchAll"
          in

          lwt origin = format_interlocutor original_origin in
          lwt destination = format_interlocutor original_destination in

          let content =
            content @ [
              br () ; br () ;
              pcdata "-----" ; br () ; br () ;
              pcdata "From: " ; pcdata origin ; br () ;
              pcdata "To: " ; pcdata destination ; br () ;
              br () ;
              pcdata original_content ]
          in
          let flat_content = ref "" in
          Printer.print_list (fun s -> flat_content := !flat_content ^ s) content ;
          lwt leader = $society(society)->leader in
          lwt reference = $message(message)->reference in
          send_message ~data ~reference:(Some reference) leader (Printf.sprintf "[Accretio] [%s] %s" society_name subject) !flat_content

      end
      in (module Mode_Specifics: MODE_SPECIFICS)
    | `Production ->
      let module Mode_Specifics (Stage_Specifics: STAGE_SPECIFICS) =
      struct

        include Stage_Specifics

        let log_info = fun fmt ->
          let source = sprintf "context-info-%d" society in
          let timestamp = Ys_time.now () in
          Printf.ksprintf
            (fun message ->
               Lwt_log.ign_info_f "society %d: %s" society message ;
               ignore_result (Logs.insert source timestamp message)) fmt

        let log_warning = fun ?exn fmt ->
          let source = sprintf "context-warning-%d" society in
          let timestamp = Ys_time.now () in
          Printf.ksprintf
            (fun message ->
               Lwt_log.ign_warning_f ?exn "society %d: %s" society message ;
               ignore_result (Logs.insert source timestamp message)) fmt

        let log_error = fun ?exn fmt ->
          let source = sprintf "context-error-%d" society in
          let timestamp = Ys_time.now () in
          Printf.ksprintf
            (fun message ->
               Lwt_log.ign_error_f ?exn "society %d: %s" society message ;
               ignore_result (Logs.insert source timestamp message)) fmt

        let send_message ?(stage=None) ?(in_reply_to=None) ?(reference=None) ?(attachments=[]) ?(data=[]) member subject content =
            let flat_content = ref "" in
            Printer.print_list (fun s -> flat_content := !flat_content ^ s) content ;

            let stage =
              match stage with
                None -> Stage_Specifics.stage
              | Some stage -> stage
            in
            let origin = Object_message.Stage stage in

            lwt uid =
              match_lwt Object_message.Store.create
                          ~society
                          ~origin
                          ~content:!flat_content
                          ~subject
                          ~destination:(Object_message.Member member)
                          ~reference:(Object_message.create_reference subject)
                          ~references:(match reference with Some reference -> [ reference ] | None -> [])
                          () with
              | `Object_already_exists (_, uid) -> return uid
              | `Object_created message -> return message.Object_message.uid
            in
            lwt reference = $message(uid)->reference in
            lwt _ =
              $society(society)<-data %% (fun store ->
                  List.fold_left
                    (fun acc (key, value) ->
                       (reference^"-"^key, value) :: acc)
                    store
                    data)
            in
            lwt _ = $society(society)<-outbox += (`Message (Object_society.({ received_on = Ys_time.now (); read = true })), uid) in
            lwt _ = Notify.api_send_message ?in_reply_to reference ~attachments society stage subject member content in
            return (Some uid)

        let message_member ~member ?(attachments=[]) ?(data=[]) ~subject ~content () =
          send_message ~data ~attachments member subject content

        let reply_to ~message ?(original_stage=false) ?(data=[]) ~content () =
          lwt subject = $message(message)->subject in
          lwt stage =
            match original_stage with
              false -> return_none
            | true ->
              match_lwt $message(message)->origin with
                Object_message.Stage stage -> return (Some stage)
              | _ -> return_none
          in
          match_lwt $message(message)->(origin, destination) with
          | Object_message.Member member, _
          | _, Object_message.Member member ->
            (match_lwt $message(message)->transport with
            | Object_message.NoTransport ->
              lwt reference = $message(message)->reference in
              send_message ~stage ~in_reply_to:(Some reference) ~data member (subject) content
            | Object_message.Email email ->
              send_message ~stage ~in_reply_to:(Some email.Object_message.message_id) ~data member (subject) content)
          | _ ->
            log_error "can't reply_to message %d" message ;
            return_none

        let forward_to_supervisor ~message ?(data=[]) ~subject ~content () =
          lwt leader = $society(society)->leader in
          let subject = Printf.sprintf "[Accretio] [%s] %s" society_name subject in
          lwt reference, original_content, attachments = $message(message)->(reference, raw, attachments) in

          lwt original_origin, original_destination = $message(message)->(origin, destination) in
          let format_interlocutor =
            function
              Object_message.Stage stage ->
              return (Printf.sprintf "Stage %s" stage)
            | Object_message.Member member ->
              lwt name, preferred_email = $member(member)->(name, preferred_email) in
              return (Printf.sprintf "Member %s <%s>" name preferred_email)
            | Object_message.CatchAll ->
              return "CatchAll"
          in

          lwt origin = format_interlocutor original_origin in
          lwt destination = format_interlocutor original_destination in

          let content =
            content @ [ br () ;
                        br () ;
                        pcdata "-----" ; br () ;
                        br () ;
                        pcdata "From: " ; pcdata origin ; br () ;
                        pcdata "To: " ; pcdata destination ; br () ;
                        br () ;
                        pcdata original_content
                      ]
          in
          let flat_content = ref "" in
          Printer.print_list (fun s -> flat_content := !flat_content ^ s) content ;
          lwt uid =
            match_lwt Object_message.Store.create
                        ~society
                          ~origin:(Object_message.Stage stage)
                          ~content:!flat_content
                          ~subject
                          ~destination:(Object_message.Member leader)
                          ~reference:(Object_message.create_reference subject)
                          ~references:[ reference ]
                          () with
              | `Object_already_exists (_, uid) -> return uid
              | `Object_created message -> return message.Object_message.uid
          in
          (* TODO: I'm not sure that we want to attach the data at this level
             as a matter of fact I'm pretty sure it's quite wrong, we probably
             want to go back to the original message - or both ?? *)
          lwt references =
            match_lwt $message(message)->references with
              [] -> lwt r =  $message(uid)->reference in return [ r ]
            | reference :: _ -> lwt r =  $message(uid)->reference in return [ r ; reference ]
          in
          lwt _ =
            $society(society)<-data %% (fun store ->
                List.fold_left
                  (fun acc (key, value) ->
                     List.fold_left
                       (fun acc reference ->
                          (reference^"-"^key, value) :: acc)
                       acc
                       references)
                  store
                  data)
          in
          lwt reference = $message(uid)->reference in
          lwt _ = Notify.api_forward_message ~attachments reference society stage subject leader message content in

          lwt _ = $society(society)<-outbox += (`Message (Object_society.({ received_on = Ys_time.now (); read = true })), uid) in
          return (Some uid)

      end
      in (module Mode_Specifics: MODE_SPECIFICS)
  in

  let module Mode_Specifics = (val mode_specifics : MODE_SPECIFICS) in

  let module Factory (Mode_Specifics: MODE_SPECIFICS) (Stage_Specifics : STAGE_SPECIFICS) =
  struct

    include Stage_Specifics
    include Mode_Specifics(Stage_Specifics)

    (* getting talent & tagging people *)

    let search_members ?max ~query () =
      Object_society.Store.search_members society query

    let tag_member ~member ~tags =
      Lwt_log.ign_info_f "tagging member %d in society %d" member society ;
      lwt _ = $society(society)<-members %% (fun edges ->
          let existing_tags =
            match Ys_uid.Edges.find member edges with
              Some (`Member tags) -> tags
            | _ -> []
          in
          Ys_uid.Edges.add_unique (`Member (Ys_list.merge ("active" :: tags) existing_tags), member) edges) in
      return_unit

    let untag_member ~member ~tags =
      Lwt_log.ign_info_f "untagging member %d in society %d" member society ;
      lwt _ = $society(society)<-members %% (fun edges ->

          let existing_tags =
            match Ys_uid.Edges.find member edges with
              Some (`Member tags) -> tags
            | _ -> []
          in
          Ys_uid.Edges.add_unique (`Member (Ys_list.remove existing_tags tags), member) edges)
      in
      return_unit

    let check_tag_member ~member ~tag =
      Lwt_log.ign_info_f "checking if member %d has tag %s in society %d" member tag society ;
      lwt members = $society(society)->members in
      return
        (List.exists
           (function
             | `Member tags, uid -> uid = member && List.mem tag tags
             | `Candidate, uid -> false) members)

    (* setting up cron jobs *)

    let set ~key ~value =
      lwt _ = $society(society)<-data %% (fun data -> (key, value) :: List.remove_assoc key data) in
      lwt _ = $society(society)<-data_keys %% (fun keys -> (`Key key, (Random.int 1024 + Int64.to_int (Ys_time.now ()))) :: (List.filter (fun (`Key k, _) -> k <> key) keys)) in
      return_unit

    let get ~key =
      lwt data = $society(society)->data in
      return
        (try Some (List.assoc key data) with Not_found -> None)

    let delete ~key =
      lwt _ = $society(society)<-data %% (fun data -> List.remove_assoc key data) in
      lwt _ = $society(society)<-data_keys %% (fun keys -> List.filter (fun (`Key k, _) -> k <> key) keys) in
      return_unit

    let search ~query =
      (* this is ugly *)
      lwt edges = Object_society.Store.search_data_keys society query in
      let edges = Ys_uid.of_list edges in
      lwt keys = $society(society)->data_keys in
      let keys = List.filter (fun (`Key key, uid) -> UidSet.mem uid edges) keys in
      return (List.map (fun (`Key key, _) -> key) keys)

    (* message primitives *)

    let get_message_content ~message =
      $message(message)->content

    let get_message_raw_content ~message =
      $message(message)->raw

    let get_message_sender ~message =
      match_lwt $message(message)->origin with
      | Object_message.Stage _ -> failwith "this email was sent by a stage"
      | Object_message.CatchAll -> failwith "this email was sent from the catchall"
      | Object_message.Member member -> return member

    (* timers *)

    let set_timer = fun ?label ~duration output ->
      try_lwt
        log_info "setting a timer" ;
        let y, m, d, s = Calendar.Period.ymds duration in
        let duration = s + (d * 24 * 3600) in
        let call = Stage_Specifics.outbound_dispatcher duration output in
        log_info "we have the call" ;
        (* this *will* deadlock if we patch the stack, we need to use the sidecar *)
        lwt _ = $society(society)<-sidecar %% (fun s -> call :: s) in
        log_info "the call is set" ;
        (* that's ugly, but it gives us sphinx features right away *)
        match label with
          None -> return_unit
        | Some label ->
          log_info "setting timer %s" label ;
          match_lwt Object_timer.Store.create
                      ~society
                      ~call
                      () with
          | `Object_already_created _ -> return_unit
          | `Object_created timer ->
            lwt _ = $society(society)<-timers += (`Label label, timer.Object_timer.uid) in
            return_unit
      with exn ->
        Lwt_log.ign_error_f ~exn "error when setting the timer" ;
        log_error ~exn "error when setting the timer" ; return_unit

    let cancel_timers = fun ~query ->
      lwt timers = Object_society.Store.search_timers society query in
      log_info "cancelling %d timers using query '%s'" (List.length timers) query ;
      lwt calls =
        Lwt_list.fold_left_s
          (fun acc uid ->
             try_lwt
               lwt call = $timer(uid)->call in
               return (call :: acc)
             with _ -> return acc)
          []
       timers in
      let timers = List.fold_left (fun acc uid -> UidSet.add uid acc) UidSet.empty timers in
      lwt _ = $society(society)<-timers %% (List.filter (fun (`Label _, timer) -> not (UidSet.mem timer timers))) in
      lwt _ = $society(society)<-tombstones %% (fun tombstones -> calls @ tombstones) in
      return_unit

    let rec get_original_message ~message =
      let rec aux message =
        match_lwt $message(message)->references with
          [] -> return_none
        | references ->
          Lwt_list.fold_left_s
            (fun acc reference ->
               match_lwt Object_message.Store.find_by_reference reference with
               | None -> return acc
               | Some message ->
                 match_lwt $message(message)->origin with
                   Object_message.Stage _ -> aux message
                 | Object_message.CatchAll -> aux message
                 | Object_message.Member _ ->
                   match_lwt aux message with
                     None -> return (Some message)
                   | Some message -> return (Some message))
            None
            references
      in
      match_lwt aux message with
        None -> return message
      | Some message -> return message

   let get_message_data ~message ~key =
     (* lwt original = get_original_message ~message in *)
     let original = message in
     Lwt_log.ign_info_f "get message data message:%d key:%s original_message:%d" message key original ;
     (* we need to find the parent of this original message *)
     lwt references = $message(original)->references in
     lwt original_outbound_message_reference =
       Lwt_list.fold_left_s
         (fun acc reference ->
            match_lwt Object_message.Store.find_by_reference reference with
            | None -> return acc
            | Some _ -> return (Some reference))
         None
         references
     in

     match original_outbound_message_reference with
     | None ->
       Lwt_log.ign_info_f "no original outbound reference for message %d, key %s" message key ;
       return_none
     | Some reference ->
       Lwt_log.ign_info_f "original outbound reference for message %d, key %s is %s" message key reference ;
       let key = reference^"-"^key in
       get ~key

   let add_member ~member =
     log_info "adding member %d" member ;
     lwt _ = $society(society)<-members +=! (`Member [ "active" ], member) in
     return_unit

   let remove_member ~member =
     log_info "removing member %d" member ;
     lwt _ = $society(society)<-members -= member in
     return_unit

   let is_member ~member =
     lwt members = $society(society)->members in
     return (Ys_uid.Edges.mem member members)

   (* message primitives *)

   let message_supervisor ~subject ?(attachments=[]) ?(data=[]) ~content () =
     lwt leader = $society(society)->leader in
     let content =
       content @ [ br () ; pcdata "The society dashboard is accessible here: " ;
                   Raw.a ~a:[ a_href (uri_of_string (fun () -> direct_link)) ] [ pcdata direct_link ] ; br () ]
     in
     message_member ~member:leader ~attachments ~subject:(Printf.sprintf "[Accretio] [%s] %s" society_name subject) ~data ~content ()

    (* payments *)

    let request_payment ~member ~label ~evidence ~amount ~on_success ~on_failure =
      log_info "requesting payment to member %d" member ;
      match_lwt Ys_shortlink.create () with
        None -> return_none
      | Some shortlink ->

        match_lwt Object_payment.Store.create
                    ~member
                    ~label
                    ~amount:(Object_payment.apply_stripe_fees amount)
                    ~currency:Object_payment.USD
                    ~society
                    ~callback_success:None
                    ~callback_failure:None
                    ~shortlink
                    ~state:Object_payment.Pending
                    () with
          `Object_already_exists _ -> Lwt_log.ign_error_f "couldn't create invoice??" ; return_none
        | `Object_created payment ->
          lwt _ = $member(member)<-payments += (`Payment, payment.Object_payment.uid) in
          let callback_success = Stage_Specifics.outbound_dispatcher 0 (on_success payment.Object_payment.uid) in
          let callback_failure = Stage_Specifics.outbound_dispatcher 0 (on_failure payment.Object_payment.uid) in

          lwt _ = $payment(payment.Object_payment.uid)<-callback_success = Some callback_success in
          lwt _ = $payment(payment.Object_payment.uid)<-callback_failure = Some callback_failure in
          lwt _ = $society(society)<-payments += (`Payment, payment.Object_payment.uid) in
          return (Some payment.Object_payment.uid)

    let payment_direct_link ~payment =
      lwt shortlink = $payment(payment)->shortlink in
      let link = (Ys_config.get_string "url-prefix")^"/payment/"^shortlink in
      return link

    let payment_amount ~payment =
      $payment(payment)->amount

    (* meta *)

    let search_societies ~query () =
      Object_society.Store.search_societies society query

    let create_society ~playbook ~name ~description () =
      log_info "creating a new society, %s playbook %s %s" name playbook description ;
        match_lwt Ys_shortlink.create () with
        None ->
        log_error "couldn't create shortlink" ;
        return_none
      | Some shortlink ->
        match_lwt Object_playbook.Store.search_name playbook with
          [] ->
          log_error "playbook %s doesn't exist" playbook ;
          return_none
        | playbook :: _ ->
           match_lwt Object_society.Store.create
                      ~shortlink
                      ~leader:society_supervisor
                      ~name
                      ~description
                      ~playbook
                      ~mode:Object_society.Public
                      ~data:[]
                      () with
           | `Object_already_exists (_, uid) -> return (Some uid)
           | `Object_created obj ->
             lwt _ = $member(society_supervisor)<-societies += (`Society, obj.Object_society.uid) in
             lwt _ = $society(society)<-societies += (`Society name, obj.Object_society.uid) in
             return (Some obj.Object_society.uid)

(* shadow message_member with timeouts *)

    let message_member ~member ?remind_after ?(attachments=[]) ?(data=[]) ~subject ~content () =
      match_lwt message_member ~member ~attachments ~data ~subject ~content () with
            None -> return_none
          | Some uid ->
            match remind_after with
              None -> return (Some uid)
            | Some duration ->
              let label = reminder_label uid in
              (* a bit hackish here *)
              let y, m, d, s = Calendar.Period.ymds duration in
              let duration = s + (d * 24 * 3600) in
              let call = {
                stage = Api.Stages.remind__ ;
                args = Deriving_Yojson.Yojson_int.to_string uid ;
                schedule = Delayed duration ;
                created_on = Ys_time.now ()
              } in
              lwt _ = $society(society)<-sidecar %% (fun s -> call :: s) in
              log_info "setting timer %s" label ;
              match_lwt Object_timer.Store.create
                            ~society
                            ~call
                            () with
              | `Object_already_created _ -> return (Some uid)
              | `Object_created timer ->
                lwt _ = $society(society)<-timers += (`Label label, timer.Object_timer.uid) in
                return (Some uid)

    (* now we finally have the context that we'll feed to the specific stage *)

    let context = {

      society ;
      society_name ;
      society_description ;
      society_supervisor ;

      direct_link ;

      stage ;

      log_info ;
      log_warning ;
      log_error ;

      set_timer ;
      cancel_timers ;

      message_member ;
      message_supervisor ;
      reply_to ;
      forward_to_supervisor ;

      search_members ;
      tag_member ;
      untag_member ;
      check_tag_member ;

      set ;
      get ;
      delete ;
      search ;

      get_message_content ;
      get_message_raw_content ;

      get_message_sender ;
      get_original_message ;

      add_member ;
      remove_member ;
      is_member ;

      get_message_data ;

      request_payment ;
      payment_direct_link ;
      payment_amount ;

      search_societies ;
      create_society ;
    }

  end in
  let module FactoryWithMode = Factory(Mode_Specifics) in
  return (module FactoryWithMode: STAGE_CONTEXT_FACTORY)

let context_factory_sandbox = context_factory `Sandbox
let context_factory_production = context_factory `Production


(* the step-by-step execution *)

let step_lock = Lwt_mutex.create ()

let step society =

  match_lwt check_missing_parameters society with
    _ :: _ as parameters ->
    (* todo: this is a big error and should be reported inside the society logs as well *)
    Lwt_log.ign_info_f "society %d isn't ready, there are %d missing parameters" society (List.length parameters) ;
    return_unit

  | [] ->

    lwt playbook, mode = $society(society)->(playbook, mode) in

    lwt context_factory =
      match mode with
      | Object_society.Sandbox ->
        Lwt_log.ign_info_f "society %d is running in mode sandbox" society ;
        context_factory_sandbox society
      | _ ->
        Lwt_log.ign_info_f "society %d is running in mode production" society ;
        context_factory_production society
    in

    let playbook = Registry.get playbook in (* here we want to revisit how we load playbooks, but fine *)
    let module Playbook = (val playbook : Api.PLAYBOOK) in

    let is_due call =
      match call.schedule with
      | Delayed delay when Int64.add call.created_on (Int64.of_int delay) > Ys_time.now () ->
        Lwt_log.ign_info_f "there is a delayed call, trigger after %d, created on %Ld, skipping" delay call.created_on ;
        false
      | Delayed delay ->
        Lwt_log.ign_info_f "there is a delayed call, trigger after %d, created on %Ld, running" delay call.created_on ;
        true
      | _ -> true
    in

    let run stack =
      try_lwt match stack with
          [] ->
          Lwt_log.ign_info_f "society %d has an empty stack" society ;
          return []
        | _ as stack ->
          let rec look_for_first_call_due = function
              [] -> return []
            | call :: tail ->
              match is_due call with
                false ->
                lwt stack = look_for_first_call_due tail in
                return (call :: stack)
              | true ->
                Lwt_log.ign_info_f "society %d is unstacking call to stage %s, args is %s" society call.Ys_executor.stage call.Ys_executor.args ;
                lwt result = Playbook.step context_factory call in
                lwt _ = $society(society)<-history %% (fun history -> (call, result) :: history) in
                match result with
                  None -> return tail
                | Some followup -> return (followup :: tail)
          in
          look_for_first_call_due stack
      with exn -> Lwt_log.ign_error_f ~exn "Error inside the stack" ; return stack
    in

    let rec unstack () =
      (* this looks weird but it's made so that it leverages the per-field mutexes defined by pa_vertex *)
      lwt _ = $society(society)<-stack %%% run in
      (* moving back all side car calls back into the main stack *)
      Lwt_log.ign_info_f "moving the sidecar back into the main stack for society %d" society ;
      lwt _ = $society(society)<-sidecar %%% (fun sidecar ->
          lwt _ = $society(society)<-stack %% (fun stack -> sidecar @ stack) in
          return [])
      in
      Lwt_log.ign_info_f "deleting tombstoned calls from the stack for society %d" society ;
      (* deleting all the inflight calls that were cancelled *)
      lwt _ = $society(society)<-stack %%% (fun stack ->
          lwt tombstones = $society(society)->tombstones in
          lwt _ = $society(society)<-tombstones = [] in
          return
            (List.filter
               (fun call -> not (List.mem call tombstones))
               stack))
      in

      match_lwt $society(society)->stack with
      | stack when List.exists is_due stack -> unstack ()
      | _ -> return_unit
    in

    unstack ()


let step society =
  try_lwt
    Lwt_mutex.with_lock
      step_lock
      (fun () -> step society)
  with exn -> Lwt_log.ign_error_f ~exn "can't step society %d" society ; return_unit

let push society call =
  lwt _ = $society(society)<-stack %% (fun calls -> call :: calls) in
  return_unit

(* the manual hooks for the UI control *)
(* the strategy here is to push the call on the stack & awake; that way it gets
   inserted inside the history, etc etc. *)

open Deriving_Yojson

let push_and_execute society call =
  lwt _ = push society call in
  ignore_result (step society) ;
  return_unit

let unit_args = Yojson_unit.to_string ()


let stack_unit society stage () =
  push society
    {
      stage ;
      args = unit_args ;
      schedule = Immediate ;
      created_on = Ys_time.now () ;
    }

let stack_and_trigger_unit society stage =
  push_and_execute society
    {
      stage ;
      args = unit_args ;
      schedule = Immediate ;
      created_on = Ys_time.now () ;
    }

let int_args i = Yojson_int.to_string i

let stack_int society stage args =
  push society
    {
      stage ;
      args = int_args args ;
      schedule = Immediate ;
      created_on = Ys_time.now () ;
    }

let stack_and_trigger_int society stage args =
  push_and_execute society
    {
      stage ;
      args = int_args args ;
      schedule = Immediate ;
      created_on = Ys_time.now () ;
    }

let stack_and_trigger_float society stage args =
  let args = Yojson_float.to_string args in
  push_and_execute society
    {
      stage ;
      args ;
      schedule = Immediate ;
      created_on = Ys_time.now () ;
    }

let stack_and_trigger_string society stage args =
  let args = Yojson_string.to_string args in
  push_and_execute society
    {
      stage ;
      args ;
      schedule = Immediate ;
      created_on = Ys_time.now () ;
    }

let stack_and_trigger_raw society stage args =
  push_and_execute society
    {
      stage ;
      args ;
      schedule = Immediate ;
      created_on = Ys_time.now () ;
    }

(* this is a naive cron executor *)

open CalendarLib

let last_minute_checked : Calendar.t option Eliom_reference.eref =
  Eliom_reference.eref
    ~scope:Eliom_common.global_scope
    ~persistent:"__mu_cron_check"
    None

let check_all_crons () =
  lwt minute =
    match_lwt Eliom_reference.get last_minute_checked with
      None ->
      let start = Calendar.now () in
      return (Calendar.rem start (Calendar.Period.second (Calendar.second start)))
    | Some m -> return (Calendar.add m (Calendar.Period.minute 1)) in

  try_lwt

  lwt to_awake, _ =
    Object_society.Store.fold_flat_lwt
    (fun acc uid ->
      try_lwt
        match_lwt $society(uid)->mode with
        | Object_society.Sandbox -> return (Some acc) (* don't wake up cron jobs for sandboxed societies *)
        | _ ->
          lwt playbook = $society(uid)->playbook in
          let playbook = Registry.get playbook in
          let module Playbook = (val playbook: Api.PLAYBOOK) in
          (* let's check each crontabs & stack up the calls if needed *)
          (* we run them all because we need to check the timeouts *)
          let need_to_be_waken_up = ref true in
          lwt _ = $society(uid)<-stack %% (fun stack ->
              List.fold_left
                (fun stack (stage, crontab) ->
                   match Cron.evaluate crontab minute with
                   | false -> stack
                   | true ->
                     need_to_be_waken_up := true ;
                     Ys_executor.({ stage ; args = unit_args ; schedule = Immediate ; created_on = Ys_time.now () }) :: stack
                )
                stack
                Playbook.crontabs) in
          match !need_to_be_waken_up with
          | false -> return (Some acc)
          | true -> return (Some (uid :: acc))
       with _ -> return (Some acc))
          []
          None
          (-1)
         in
         Lwt_log.ign_info_f "awaking %d society because they have active crons" (List.length to_awake) ;
         lwt _ = Lwt_list.iter_p step to_awake in
         lwt _ = Eliom_reference.set last_minute_checked (Some minute) in
         return minute
with exn -> Lwt_log.ign_error_f ~exn "exception caught when running crons" ; return minute


(* This could loop if there is a loop in the message graph *)
let cancel_reminders society message =
  Lwt_log.ign_info_f "recieved message %d, cancelling all reminders for all referenced messages" message ;
  let rec cancel message =
    let label = reminder_label message in
    lwt timers = Object_society.Store.search_timers society label in
    lwt calls =
      Lwt_list.fold_left_s
        (fun acc uid ->
           try_lwt
             lwt call = $timer(uid)->call in
             return (call :: acc)
           with _ -> return acc)
          []
       timers in
    let timers = List.fold_left (fun acc uid -> UidSet.add uid acc) UidSet.empty timers in
    lwt _ = $society(society)<-timers %% (List.filter (fun (`Label _, timer) -> not (UidSet.mem timer timers))) in
    lwt _ = $society(society)<-stack %% (fun stack ->
                 List.filter
                   (fun call -> not (List.mem call calls))
                   stack)
    in
    lwt references = $message(message)->references in
    Lwt_list.iter_s
      (fun reference ->
         match_lwt Object_message.Store.find_by_reference reference with
           None -> return_unit
         | Some message -> cancel message)
      references
  in
  cancel message

let rec start_cron () =
  lwt next_minute =
    try_lwt
      check_all_crons ()
    with exn ->
      Lwt_log.ign_error_f ~exn "cron error" ;
      Lwt.fail exn
  in
  let now = Calendar.now () in

  Lwt_log.ign_info_f "Running crons, %s %s" (Printer.Calendar.sprint "now: %c" now) (Printer.Calendar.sprint "next_minute: %c" next_minute) ;
  if next_minute < now then
    start_cron ()
  else
    let diff = Calendar.sub next_minute now in
    let y,m,d,s = Calendar.Period.ymds diff in
    Lwt_log.ign_info_f "next diff in %d %d %d %d" y m d s ;
    lwt _ = Lwt_unix.sleep (float_of_int s) in
    start_cron ()

let _ =
  ignore_result (Eliom_reference.set last_minute_checked None)
