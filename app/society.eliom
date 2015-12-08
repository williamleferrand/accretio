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


{shared{

open Lwt
open Ys_uid
open Vault
open View_society


type bundle =
  {
    uid : uid ;
    view : View_society.t ;
    (* playbook info *)
    automata : string ;
    triggers :  ([ `Unit | `Int | `Float | `String ] * string) list ;
    mailables : string list ;
    (* society info *)
    members : (View_member.t * string list) list ;
    data : (string * string) list ;
  }

}}

{server{

open Vault

let retrieve_members uid =
  lwt members = $society(uid)->members in
  Lwt_list.fold_left_s
    (fun acc -> function
         | `Member tags, uid ->
           lwt view = View_member.to_view uid in
           return ((view, tags) :: acc)
         | _ -> return acc)
      []
      members

let retrieve uid =
  lwt view = View_society.to_view uid in
  lwt members = retrieve_members uid in
  lwt data, playbook = $society(uid)->(data, playbook) in
  let automata, triggers, mailables =
    let playbook = Registry.get playbook in
    let module P = (val playbook : Api.PLAYBOOK) in
    P.automata, P.triggers, P.mailables
  in
  return
    {
      uid ;
      view ;
      automata ;
      triggers ;
      mailables ;
      members ;
      data ;
    }

let retrieve = server_function ~name:"society-retrieve" Json.t<int> retrieve

let update_mode (uid, mode) =
  protected_connected
    (fun session ->
       (* only the leader can change the status for now *)
       lwt leader = $society(uid)->leader in
       match session.member_uid = leader with
        | false -> return_none
        | true ->
          let mode' =
            match mode with
              Sandbox -> Object_society.Sandbox
            | Public -> Object_society.Public
            | Private -> Object_society.Private in
          $society(uid)<-mode = mode' ;
          return (Some mode))

let update_mode = server_function ~name:"society-update-mode" Json.t<int * mode> update_mode

(* logs  *)

let get_logs (society, since) =
  Lwt_log.ign_info_f "retrieving logs for society %d, since %Ld" society since ;
  Logs.list_all_from_society society since

let get_logs = server_function ~name:"society-get-logs" Json.t<int * int64> get_logs

(* history & stack *)

let get_stack society =
  $society(society)->stack

let get_stack = server_function ~name:"society-get-stack" Json.t<int> get_stack

let reset_stack society =
  lwt stack = $society(society)<-stack %%% (fun _ -> return []) in
  return (Some stack)

let reset_stack = server_function ~name:"society-reset-stack" Json.t<int> reset_stack

(* triggers, todo ACL *)

let trigger_unit (society, stage) =
  Lwt_log.ign_info_f "triggering stage unit, society is %d, stage is %s" society stage ;
  Executor.stack_and_trigger_unit society stage

let trigger_int (society, stage, args) =
  Lwt_log.ign_info_f "triggering stage int, society is %d, stage is %s, args is %d" society stage args ;
  Executor.stack_and_trigger_int society stage args

let trigger_float (society, stage, args) =
  Lwt_log.ign_info_f "triggering stage float, society is %d, stage is %s, args is %f" society stage args ;
  Executor.stack_and_trigger_float society stage args

let trigger_string (society, stage, args) =
  Lwt_log.ign_info_f "triggering stage string, society is %d, stage is %s, args is %s" society stage args ;
  Executor.stack_and_trigger_string society stage args

let trigger_unit = server_function ~name:"society-trigger-unit" Json.t<int * string> trigger_unit
let trigger_int = server_function ~name:"society-trigger-int" Json.t<int * string * int> trigger_int
let trigger_float = server_function ~name:"society-trigger-float" Json.t<int * string * float> trigger_float
let trigger_string = server_function ~name:"society-trigger-string" Json.t<int * string * string> trigger_string

(* members *)

let add_member (uid, email, tags) =
  protected_connected
    (fun _ ->
       lwt uid =
         match_lwt Object_member.Store.find_by_email email with
         | Some uid -> return uid
         | None ->
           match_lwt Object_member.Store.create
                       ~preferred_email:email
                       ~emails:[ email ]
                       () with
           | `Object_already_exists (_, uid) -> return uid
           | `Object_created member -> return member.Object_member.uid
       in
       let tags = "active" :: tags in
       lwt _ = $society(uid)<-members +=! (`Member tags, uid) in
       lwt members = retrieve_members uid in
       return (Some members))

let add_member = server_function ~name:"society-add-member" Json.t<int * string * string list> add_member

let remove_member (uid, member) =
  protected_connected
    (fun _ ->
       lwt _ = $society(uid)<-members -= member in
       lwt members = retrieve_members uid in
       return (Some members))

let remove_member = server_function ~name:"society-remove-member" Json.t<int * int> remove_member

let update_member_tags (uid, member, tags) =
  let tags = "active" :: tags in
  protected_connected
    (fun _ ->
       lwt _ = $society(uid)<-members -= member in
       lwt _ = $society(uid)<-members += (`Member tags, member) in
       lwt members = retrieve_members uid in
       return (Some members))

let update_member_tags = server_function ~name:"society-update-member-tags" Json.t<int * int * string list> update_member_tags

(* mailbox *)

let get_messages_inbox (society, since) =
  lwt inbox = $society(society)->inbox in
  let inbox = List.filter (fun (`Message slip, _) -> slip.Object_society.received_on > since) inbox in
  let inbox = List.rev inbox in
  Lwt_list.map_p (fun (`Message slip, uid) -> lwt view = View_message.to_view uid in return (slip.Object_society.received_on, view)) inbox

let get_messages_inbox = server_function ~name:"society-get-messages-inbox" Json.t<int * int64> get_messages_inbox

let get_messages_outbox (society, since) =
  lwt outbox = $society(society)->outbox in
  let outbox = List.filter (fun (`Message slip, _) -> slip.Object_society.received_on > since) outbox in
  let outbox = List.rev outbox in
  Lwt_list.map_p (fun (`Message slip, uid) -> lwt view = View_message.to_view uid in return (slip.Object_society.received_on, view)) outbox

let get_messages_outbox = server_function ~name:"society-get-messages-outbox" Json.t<int * int64> get_messages_outbox

let reply_message (society, message, content) =
  protected_connected
    (fun _ ->
       match_lwt $message(message)->origin with
        | Object_message.Member _ -> return (Some false)
        | Object_message.Stage stage as destination ->
          lwt origin = $message(message)->destination in
          lwt uid =
            match_lwt Object_message.Store.create
                        ~origin
                        ~society
                        ~content
                        ~destination
                        () with
            | `Object_already_exists (_, uid) -> return uid
            | `Object_created message -> return message.Object_message.uid in
          lwt _ = $society(society)<-inbox += (`Message (Object_society.{ received_on = Ys_time.now () ; read = true }), uid) in
          let call = Ys_executor.({ stage ; args = Executor.int_args uid ; schedule = Immediate }) in
          lwt _ = $society(society)<-stack %% (fun stack -> call :: stack) in
          ignore_result (Executor.step society) ;
          (* Scheduler.dispatch_message society uid ; *)
          return (Some true))

let reply_message = server_function ~name:"society-reply-message" Json.t<int * int * string> reply_message

let create_message_and_trigger_stage (society, sender, content, stage) =
  Lwt_log.ign_info_f "creating message for society %d, sender is %d, content is %s, stage is %s" society sender content stage ;
  protected_connected
    (fun _ ->
       lwt uid =
         match_lwt Object_message.Store.create
                     ~origin:(Object_message.Member sender)
                     ~society
                     ~content
                     ~destination:(Object_message.Stage stage) (* !!!!! this is not really correct! *)
                     () with
         | `Object_already_exists (_, uid) -> return uid
         | `Object_created message -> return message.Object_message.uid in
       lwt _ = $society(society)<-inbox += (`Message (Object_society.{ received_on = Ys_time.now () ; read = true }), uid) in
       let call = Ys_executor.({ stage ; args = Executor.int_args uid ; schedule = Immediate }) in
       lwt _ = $society(society)<-stack %% (fun stack -> call :: stack) in
       ignore_result (Executor.step society) ;
       return (Some ()))

let create_message_and_trigger_stage = server_function ~name:"create-message-and-trigger-stage" Json.t<int * int * string * string> create_message_and_trigger_stage

(* store *)

let set_value (society, key, value_option) =
  protected_connected
    (fun _ ->
       lwt _ = $society(society)<-data %%
                                    (fun data ->
                                       match value_option with
                                         None -> List.remove_assoc key data
                                       | Some v -> (key, v) :: List.remove_assoc key data) in
       lwt data = $society(society)->data in
       return (Some data))

let set_value = server_function ~name:"society-set-value" Json.t<int * string * string option> set_value

(* tick the recipe manually *)

let tick society =
  protected_connected
    (fun _ ->
       ignore_result (Executor.step society) ;
       return (Some ()))

let tick = server_function ~name:"society-tick" Json.t<int> tick

}}

{client{

open React
open Ys_react
open Eliom_content.Html5
open Eliom_content.Html5.D
open View_society

let refresh_rate = 1.0

let attach society container fetcher viewer =
  let rec fetch since =
    let container = To_dom.of_div container in
    lwt data = fetcher (society, since) in

    let since =
      List.fold_left
        (fun _ (timestamp, data) ->
           Dom.insertBefore container (To_dom.of_div (viewer data)) (container##firstChild) ;
           timestamp)
        since
        data
    in
    lwt _ = Lwt_js.sleep refresh_rate in
    fetch since
  in
  ignore_result
    (lwt _ = Lwt_js.yield () in fetch (Ys_timeago.now ()))

let attach_logs society logs =
  attach society logs (%get_logs) (fun (msg, ts, source) -> div ~a:[ a_class [ "log" ; source ]] [ Ys_timeago.format ts ; pcdata ": " ; pcdata msg ])

let attach_inbox society inbox =
  attach society inbox (%get_messages_inbox) View_message.format

let attach_stack society holder =
  let rec fetch () =
    lwt stack = %get_stack society in
    RList.update holder stack ;
    lwt _ = Lwt_js.sleep refresh_rate in
    fetch ()
  in
  ignore_result
    (lwt _ = Lwt_js.yield () in fetch ())

let attach_outbox society outbox =
 let format message =
    let textarea_reply = Raw.textarea (pcdata "") in
    let reply _ =
      match Ys_dom.get_value_textarea textarea_reply with
        "" -> Help.warning "Please say something"
      | _ as reply ->
        Authentication.if_connected
          (fun _ -> rpc %reply_message (society, message.View_message.uid, reply) (fun _ -> Ys_dom.set_value_textarea textarea_reply ""))
    in
    let reply =
      button
        ~button_type:`Button
        ~a:[ a_onclick reply ]
        [ pcdata "Reply" ]
    in
    div ~a:[ a_class [ "message-outbox" ]] [
      View_message.format message ;
      textarea_reply ;
      reply ;
    ]

  in
  attach society outbox (%get_messages_outbox) format

let builder bundle =
  let view = bundle.view in

  (* toggling the mode should have an impact on the scheduler *)

  let mode =
    let mode, update_mode = S.create view.mode in
    let update_mode mode =
      detach_rpc %update_mode (view.uid, mode) update_mode
    in
    let mode_toggle m l =
      button
        ~button_type:`Button
        ~a:[ a_onclick (fun _ -> update_mode m) ;
             R.a_class (S.map (function mode -> if mode = m then [ "active" ] else []) mode) ]
        [ pcdata l ]
    in
    div ~a:[ a_class [ "society-mode" ]] [
      mode_toggle Sandbox "Sandbox" ;
      mode_toggle Public "Public" ;
      mode_toggle Private "Private" ;
    ]
  in

  (* displaying the logs more or less in RT *)

  let logs = div ~a:[ a_class [ "logs-inner" ]] [] in

  attach_logs view.uid logs ;

  (* displaying the mailbox more or less in RT *)

  let inbox = div ~a:[ a_class [ "inbox" ]] [] in
  let outbox = div ~a:[ a_class [ "outbox" ]] [] in

  attach_inbox view.uid inbox ;
  attach_outbox view.uid outbox ;

  (* the stack *)

  let stack = RList.create () in
  attach_stack view.uid stack ;


  let format_call call =
    div ~a:[ a_class [ "call" ]] [
      pcdata call.Ys_executor.stage
    ]
  in

  let tick =
    let tick _ =
      Authentication.if_connected
        (fun _ -> rpc %tick view.uid (fun () -> ()))
    in
    let tick =
      button
        ~button_type:`Button
        ~a:[ a_onclick tick ]
        [ pcdata "Tick" ]
    in
    let reset _ =
      Authentication.if_connected
        (fun _ -> rpc %reset_stack view.uid (RList.update stack))
    in
    let reset =
      button
        ~button_type:`Button
        ~a:[ a_onclick reset ]
        [ pcdata "Reset" ]
    in
    div ~a:[ a_class [ "stack-control" ]] [
      tick ; reset ;
    ]
  in

  let stack = RList.map_in_div_hd ~a:[ a_class [ "stack" ]] tick format_call stack in

  (* the graph *)

  let graph = div ~a:[ a_class [ "playbook-automata" ; "left" ]] [] in
  Ys_viz.render graph bundle.automata ;

  (* the triggers *)

  let triggers =
    List.map
      (function (ctyp, stage) ->
        match ctyp with
        | `Unit ->
          let call =
            button
              ~button_type:`Button
              ~a:[ a_onclick (fun _ -> ignore_result (%trigger_unit (view.uid, stage))) ]
              [ pcdata stage ]
          in
          div ~a:[ a_class [ "trigger" ]] [ call ]
        | `Int ->
          let input = input ~input_type:`Number () in
          let call _ =
            let arg = Ys_dom.get_value input in
            ignore_result (%trigger_int (view.uid, stage, int_of_string arg))
          in
          let call =
            button
              ~button_type:`Button
              ~a:[ a_onclick call ]
              [ pcdata stage ]
          in
          div ~a:[ a_class [ "trigger" ]] [ input ; call  ]
        | `Float ->
          let input = input ~input_type:`Text () in
          let call _ =
            let arg = Ys_dom.get_value input in
            ignore_result (%trigger_float (view.uid, stage, float_of_string arg))
          in
          let call =
            button
              ~button_type:`Button
              ~a:[ a_onclick call ]
              [ pcdata stage ]
             in
             div ~a:[ a_class [ "trigger" ]] [ input ; call ]
        | `String ->
          let input = input ~input_type:`Text () in
          let call _ =
            let arg = Ys_dom.get_value input in
            ignore_result (%trigger_string (view.uid, stage, arg))
          in
          let call =
            button
                 ~button_type:`Button
                 ~a:[ a_onclick call ]
                 [ pcdata stage ]
          in
          div ~a:[ a_class [ "trigger" ]] [ input ; call ])
      bundle.triggers
  in

  (* members *)

  let members =

    let members = RList.init bundle.members in

    let format_member (member, tags) =

      let tag_input = input ~a:[ a_placeholder "Comma separated tags" ; a_value (String.concat ", " tags) ] ~input_type:`Text () in
      let tag_update _ =
        let tags = Ys_dom.get_value tag_input in
        let tags = Regexp.split (Regexp.regexp ",") tags in
        Authentication.if_connected
          (fun _ -> rpc %update_member_tags (view.uid, member.View_member.uid, tags) (RList.update members))
      in
      let tag_update =
        button
          ~button_type:`Button
          ~a:[ a_onclick tag_update ]
          [ pcdata "Update tags" ]
      in

      let remove _ =
        Authentication.if_connected
          (fun _ -> rpc %remove_member (view.uid, member.View_member.uid) (RList.update members))
      in
      let remove =
        button
          ~button_type:`Button
          ~a:[ a_onclick remove ]
          [ pcdata "Remove" ]
      in

      div ~a:[ a_class [ "member" ]] [
        span ~a:[ a_class [ "member-name" ]] [ pcdata member.View_member.name ] ;
        span ~a:[ a_class [ "member-email" ]] [ pcdata member.View_member.email ] ;
        tag_input ;
        tag_update ;
        remove ;
      ]
    in

    let add_member =
      let input_email = input ~a:[ a_placeholder "Email" ] ~input_type:`Text () in
      let input_tags = input ~a:[ a_placeholder "Comma separated tags" ] ~input_type:`Text () in
      let create _ =
        match Ys_dom.get_value input_email with
          "" -> Help.warning "Please specify a valid email"
        | _ as email ->
          let tags = Ys_dom.get_value input_tags in
          let tags : string list = Regexp.split (Regexp.regexp ",") tags in
          Authentication.if_connected
            (fun _ -> rpc %add_member (view.uid, email, tags) (fun m -> RList.update members m ; Ys_dom.set_value input_email "" ; Ys_dom.set_value input_tags ""))
      in
      let create =
        button
          ~button_type:`Button
          ~a:[ a_onclick create ]
          [ pcdata "Create" ]
      in
      div ~a:[ a_class [ "add-member" ; "left" ]] [
        input_email ; input_tags ; create
      ]
    in

    div ~a:[ a_class [ "members" ]] [
      h2 [ pcdata "Members" ] ;
      RList.map_in_div_hd ~a:[ a_class [ "members-existing" ; "clearfix" ]] add_member format_member members ;
    ]

  in

  (* the messenger *)

  let messenger =
    match bundle.mailables with
      [] -> pcdata ""
    | _ as stages ->

      let textarea = Raw.textarea ~a:[ a_placeholder "Enter your message here" ] (pcdata "") in

      let stages = Raw.select (List.map (fun stage -> option (pcdata stage)) stages) in

      let send _ =
        match Ys_dom.get_value_textarea textarea, Ys_dom.get_value_select stages with
          "", _ -> Help.warning "please write something"
        | content, stage ->
          Authentication.if_connected
            (fun session ->
               rpc %create_message_and_trigger_stage (view.uid, session.member_uid, content, stage) (fun _ -> Ys_dom.set_value_textarea textarea ""))
      in

      let send =
        button
          ~button_type:`Button
          ~a:[ a_onclick send ]
          [ pcdata "Send" ]
      in

      div ~a:[ a_class [ "messenger" ]] [
        h2 [ pcdata "Stage messenger" ] ;
        textarea ;
        div ~a:[ a_class [ "messenger-controls" ]] [ stages ; send ]
      ]
  in

  (* the little data store *)

  let data =
    let data = RList.init bundle.data in
    let add_data =
      let input_key = input ~input_type:`Text ~a:[ a_placeholder "Key" ] () in
      let input_value = input ~input_type:`Text ~a:[ a_placeholder "Value" ] () in
      let add _ =
        match Ys_dom.get_value input_key with
          "" -> Help.warning "Please specify a key"
        | _ as key ->
          Authentication.if_connected
            (fun _ ->
               rpc
               %set_value
                   (view.uid, key, Some (Ys_dom.get_value input_value))
                   (fun d -> RList.update data d ; Ys_dom.set_value input_key "" ; Ys_dom.set_value input_value ""))
      in
      let add =
        button
          ~button_type:`Button
          ~a:[ a_onclick add ]
          [ pcdata "Add" ]
      in
      div ~a:[ a_class [ "add-data" ; "left" ]] [
        input_key ;
        input_value ;
        add ;
      ]
    in
    let format_data (key, value) =
      let remove _ =
        Authentication.if_connected
          (fun _ -> rpc %set_value (view.uid, key, None) (RList.update data))
      in
      let remove =
        button
          ~button_type:`Button
          ~a:[ a_onclick remove ]
          [ pcdata "Remove" ]
      in
      div ~a:[ a_class [ "data-item" ; "left" ]] [
        pcdata key ; pcdata " -> " ; pcdata value ;
        remove ;
      ]
    in
    div ~a:[ a_class [ "data" ]] [
      h2 [ pcdata "Data" ] ;
      RList.map_in_div_hd ~a:[ a_class [ "clearfix" ]] add_data format_data data
    ]
  in

  (* the main dom *)

  div ~a:[ a_class [ "society" ]] [
    mode ;
    div ~a:[ a_class [ "society-upper" ; "clearfix" ]] [
      div ~a:[ a_class [ "logs"; "left" ]] [  stack ; logs ] ;
      graph ;
      div ~a:[ a_class [ "mailboxes" ; "left" ]] [
        inbox ;
        outbox
      ] ;
    ] ;
    div ~a:[ a_class [ "society-lower" ]] [
      div ~a:[ a_class [ "triggers" ]] [
        h2 [ pcdata "Triggers" ] ;
        div ~a:[ a_class [ "triggers-controls"; "clearfix" ]] triggers
      ] ;
      members ;
      messenger ;
      data ;
    ]
  ]

let dom = Template.apply %retrieve builder

}}
