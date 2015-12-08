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

open Eliom_content.Html5
open Eliom_content.Html5.D

open Ys_uid
open Ys_executor

open Api

(* the context factory -- there is a little bit a first class module magic
   so that pa_playbook can craft a context specific to each module *)

let context_factory_sandbox society =
  let module Factory (Stage_Specifics : STAGE_SPECIFICS) =
  struct

    include Stage_Specifics

    let log_info = fun fmt ->
      let source = sprintf "context-info-%d" society in
      let timestamp = Ys_time.now () in
      Printf.ksprintf
        (fun message ->
           Lwt_log.ign_info_f "%s" message ;
           ignore_result (Logs.insert source timestamp message)) fmt

    let log_warning = fun ?exn fmt ->
      let source = sprintf "context-warning-%d" society in
      let timestamp = Ys_time.now () in
      Printf.ksprintf
        (fun message ->
           Lwt_log.ign_warning_f ?exn "%s" message ;
           ignore_result (Logs.insert source timestamp message)) fmt

    let log_error = fun ?exn fmt ->
      let source = sprintf "context-error-%d" society in
      let timestamp = Ys_time.now () in
      Printf.ksprintf
        (fun message ->
           Lwt_log.ign_error_f ?exn "%s" message ;
           ignore_result (Logs.insert source timestamp message)) fmt

    (* messaging utilities *)

    let send_message member subject content =
      let origin = Object_message.Stage Stage_Specifics.stage in
      lwt uid =
        match_lwt Object_message.Store.create
                    ~society
                    ~origin
                    ~content
                    ~subject
                    ~destination:(Object_message.Member member)
                    () with
        | `Object_already_exists (_, uid) -> return uid
        | `Object_created message -> return message.Object_message.uid
      in
      lwt _ = $society(society)<-outbox += (`Message (Object_society.({ received_on = Ys_time.now (); read = true })), uid) in
      return_unit

    let message_member ~member ~subject ~content =
      let flat_content = ref "" in
      Printer.print_list (fun s -> flat_content := !flat_content ^ s) content ;
      send_message member subject !flat_content

    let message_supervisor ~subject ~content =
      lwt leader = $society(society)->leader in
      message_member leader subject content

    let forward_to_supervisor ~message ~subject =
      lwt content = $message(message)->content in
      lwt leader = $society(society)->leader in
      send_message leader subject content

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
          Ys_uid.Edges.add_unique (`Member (Ys_list.merge tags existing_tags), member) edges) in
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

    (* setting up cron jobs *)

    let set ~key ~value =
      $society(society)<-data %% (fun data -> (key, value) :: List.remove_assoc key data)

    let get ~key =
      lwt data = $society(society)->data in
      return
        (try Some (List.assoc key data) with Not_found -> None)

    (* message primitives *)
    let get_message_content ~message =
      $message(message)->content

    let get_message_senders ~message =
      return []

    (* timers *)

    let set_timer = fun ?label ~duration output ->
      let call = Stage_Specifics.outbound_dispatcher duration output in
      ignore_result ($society(society)<-stack %% (fun s -> call :: s)) ;
      return_unit

    let cancel_timers = fun ~query -> return_unit

    (* now we finally have the context that we'll feed to the specific stage *)

    let context = {
      stage ;

      log_info ;
      log_warning ;
      log_error ;

      set_timer ;
      cancel_timers ;

      message_member ;
      message_supervisor ;
      forward_to_supervisor ;

      search_members ;
      tag_member ;
      untag_member ;

      set ;
      get ;

      get_message_content ;
      get_message_senders ;
    }

  end in (module Factory: STAGE_CONTEXT_FACTORY)

let context_factory_production society =
  let module Factory (Stage_Specifics : STAGE_SPECIFICS) =
  struct

    include Stage_Specifics

    let log_info = fun fmt ->
      let source = sprintf "context-info-%d" society in
      let timestamp = Ys_time.now () in
      Printf.ksprintf
        (fun message ->
           Lwt_log.ign_info_f "%s" message ;
           ignore_result (Logs.insert source timestamp message)) fmt

    let log_warning = fun ?exn fmt ->
      let source = sprintf "context-warning-%d" society in
      let timestamp = Ys_time.now () in
      Printf.ksprintf
        (fun message ->
           Lwt_log.ign_warning_f ?exn "%s" message ;
           ignore_result (Logs.insert source timestamp message)) fmt

    let log_error = fun ?exn fmt ->
      let source = sprintf "context-error-%d" society in
      let timestamp = Ys_time.now () in
      Printf.ksprintf
        (fun message ->
           Lwt_log.ign_error_f ?exn "%s" message ;
           ignore_result (Logs.insert source timestamp message)) fmt

    (* messaging utilities *)

    let send_message member subject content =
      match next_mailbox with
        None ->
        log_error "can't send message to member %d, subject %s, there is no `Message transition on this stage" member subject ;
        return_unit
      | Some destination ->
        let flat_content = ref "" in
        Printer.print_list (fun s -> flat_content := !flat_content ^ s) content ;

        let origin = Object_message.Stage Stage_Specifics.stage in
        lwt uid =
          match_lwt Object_message.Store.create
                      ~society
                      ~origin
                      ~content:!flat_content
                      ~subject
                      ~destination:(Object_message.Member member)
                      () with
          | `Object_already_exists (_, uid) -> return uid
          | `Object_created message -> return message.Object_message.uid
        in
        lwt _ = $society(society)<-outbox += (`Message (Object_society.({ received_on = Ys_time.now (); read = true })), uid) in
        lwt _ = Notify.api_send_message society destination subject member content in
        return_unit

    let message_member ~member ~subject ~content =
      send_message member subject content

    let message_supervisor ~subject ~content =
      lwt leader = $society(society)->leader in
      message_member leader subject content

    let forward_to_supervisor ~message ~subject =
      lwt content = $message(message)->content in
      lwt leader = $society(society)->leader in
      send_message leader subject [ pcdata content ]

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
          Ys_uid.Edges.add_unique (`Member (Ys_list.merge tags existing_tags), member) edges) in
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

    (* setting up cron jobs *)

    let set ~key ~value =
      $society(society)<-data %% (fun data -> (key, value) :: List.remove_assoc key data)

    let get ~key =
      lwt data = $society(society)->data in
      return
        (try Some (List.assoc key data) with Not_found -> None)

    (* message primitives *)
    let get_message_content ~message =
      $message(message)->content

    let get_message_senders ~message =
      return []

    (* timers *)

    let set_timer = fun ?label ~duration output ->
      let call = Stage_Specifics.outbound_dispatcher duration output in
      ignore_result ($society(society)<-stack %% (fun s -> call :: s)) ;
      return_unit

    let cancel_timers = fun ~query -> return_unit

    (* now we finally have the context that we'll feed to the specific stage *)

    let context = {
      stage ;

      log_info ;
      log_warning ;
      log_error ;

      set_timer ;
      cancel_timers ;

      message_member ;
      message_supervisor ;
      forward_to_supervisor ;

      search_members ;
      tag_member ;
      untag_member ;

      set ;
      get ;

      get_message_content ;
      get_message_senders ;
    }

  end in (module Factory: STAGE_CONTEXT_FACTORY)

(* the step-by-step execution *)

let step society =

  lwt playbook, mode = $society(society)->(playbook, mode) in

  let context_factory =
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

  let run =
    function
      [] ->
      Lwt_log.ign_info_f "society %d has an empty stack" society ;
      return []
    | call :: tail ->

      Lwt_log.ign_info_f "society %d is unstacking call to stage %s, args is %s" society call.Ys_executor.stage call.Ys_executor.args ;

      lwt result = Playbook.step context_factory call in
      lwt _ = $society(society)<-history %% (fun history -> (call, result) :: history) in
      match result with
        None -> return tail
      | Some followup -> return (followup :: tail)

  in

  let rec unstack () =
    match_lwt $society(society)<-stack %%% run with
      [] -> return_unit
    | _ -> unstack ()
  in

  unstack ()


let push society call =
  lwt _ = $society(society)<-stack %% (fun calls -> call :: calls) in
  return_unit

(* the manual hooks for the UI control *)
(* the strategy here is to push the call on the stack & awake; that way it gets
   inserted inside the history, etc etc. *)

open Bin_prot.Std

let push_and_execute society call =
  lwt _ = push society call in
  ignore_result (step society) ;
  return_unit

let unit_args =
  let size = bin_size_unit () in
  let buf = Bin_prot.Common.create_buf size in
  let s = Bytes.create size in
  ignore (bin_write_unit buf () ~pos:0) ;
  Bin_prot.Common.blit_buf_string buf s size ;
  s

let stack_and_trigger_unit society stage =
  push_and_execute society
    {
      stage ;
      args = unit_args ;
      schedule = Immediate
    }

let int_args i =
  let size = bin_size_int i in
  let buf = Bin_prot.Common.create_buf size in
  let s = Bytes.create size in
  ignore (bin_write_int buf i ~pos:0) ;
  Bin_prot.Common.blit_buf_string buf s size ;
  s

let stack_and_trigger_int society stage args =
  push_and_execute society
    {
      stage ;
      args = int_args args ;
      schedule = Immediate ;
    }

let stack_and_trigger_float society stage args =
  let size = bin_size_float args in
  let buf = Bin_prot.Common.create_buf size in
  let s = Bytes.create size in
  ignore (bin_write_float buf args ~pos:0) ;
  Bin_prot.Common.blit_buf_string buf s size ;
  push_and_execute society
    {
      stage ;
      args = s ;
      schedule = Immediate ;
    }

let stack_and_trigger_string society stage args =
  let size = bin_size_string args in
  let buf = Bin_prot.Common.create_buf size in
  let s = Bytes.create size in
  ignore (bin_write_string buf args ~pos:0) ;
  Bin_prot.Common.blit_buf_string buf s size ;
  push_and_execute society
    {
      stage ;
      args = s ;
      schedule = Immediate ;
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
  lwt to_awake, _ =
    Object_society.Store.fold_flat_lwt
    (fun acc uid ->
      match_lwt $society(uid)->mode with
        | Object_society.Sandbox -> return (Some acc) (* don't wake up cron jobs for sandboxed societies *)
        | _ ->
          lwt playbook = $society(uid)->playbook in
          let playbook = Registry.get playbook in
          let module Playbook = (val playbook: Api.PLAYBOOK) in
          (* let's check each crontabs & stack up the calls if needed *)
          let need_to_be_waken_up = ref false in
          lwt _ = $society(uid)<-stack %% (fun stack ->
              List.fold_left
                (fun stack (stage, crontab) ->
                   match Cron.evaluate crontab minute with
                   | false -> stack
                   | true ->
                     need_to_be_waken_up := true ;
                     Ys_executor.({ stage ; args = unit_args ; schedule = Immediate }) :: stack
                )
                stack
                Playbook.crontabs) in
          match !need_to_be_waken_up with
          | false -> return (Some acc)
          | true -> return (Some (uid :: acc)))
          []
          None
          (-1)
         in
         Lwt_log.ign_info_f "awaking %d society because they have active crons" (List.length to_awake) ;
         lwt _ = Lwt_list.iter_p step to_awake in
         lwt _ = Eliom_reference.set last_minute_checked (Some minute) in
         return minute

let rec start_cron () =
  lwt next_minute = check_all_crons () in
  let now = Calendar.now () in
  Printer.Calendar.print "now: %c\n" now ;
  Printer.Calendar.print "next_minute: %c\n" next_minute ;
  print_endline "---" ;
  if next_minute < now then
    start_cron ()
  else
    let diff = Calendar.sub next_minute now in
    let y,m,d,s = Calendar.Period.ymds diff in
    Lwt_log.ign_info_f "next diff in %d %d %d %d" y m d s ;
    lwt _ = Lwt_unix.sleep (float_of_int s) in
    start_cron ()

let  _ =
  (* lwt _ =   Eliom_reference.set last_minute_checked None in *)
  start_cron ()
