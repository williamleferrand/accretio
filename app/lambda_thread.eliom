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
open Vault
open View_thread

}}

{server{

let thread_add_message session uid message =
  lwt _ = $thread(uid)<-messages %% (fun messages -> message :: messages) in
  lwt _ = $thread(uid)<-number_of_messages %% (fun number -> number + 1) in
  lwt _ = $thread(uid)<-followers %% (fun followers -> (`Member, session.member_uid) :: List.filter (fun edge -> snd edge <> session.member_uid) followers) in
  lwt context = $thread(uid)->context in
  lwt _ =
    Lwt_list.iter_p
      (function (`Cohort, uid) ->
        lwt _ = $member(session.member_uid)<-mask +=! (`Cohort 1, uid) in
        (* $cohort(uid)<-members +=! (`Member, session.member_uid) *) return_unit)
      context in
  try_lwt
    lwt _ = Notify.new_message uid message in
    lwt message = View_thread.convert_message message in
    let message =
      {
        message
        with author = { message.author with View_member.is_loved = true } } in
    Live.notify (Live.ThreadNewMessage (uid, message)) ;
    return (Some message)
  with exn ->
    Lwt_log.ign_error_f "error: %s" (Printexc.to_string exn) ;
    return_none

let thread_add_message_text ((uid, text) : (int * string)) =
  protected_connected
    (fun session ->
       let message =
         Object_thread.({
             author = session.member_uid ;
             timestamp = Ys_time.now () ;
             content = Text text
           }) in
       thread_add_message session uid message)

let thread_add_message_image ((uid, data) : (int * string)) =
  protected_connected
    (fun session ->
       match_lwt Ys_imagemagick.convert_from_b64 data with
         None -> return_none
       | Some file ->
         match_lwt Object_image.Store.create
                     ~owner:session.member_uid
                     ~content:(Object_image.File file)
                     () with
         | `Object_already_exists _ -> return_none
         | `Object_created image ->
           let message =
             Object_thread.({
                 author = session.member_uid ;
                 timestamp = Ys_time.now () ;
                 content = Image image.Object_image.uid
               }) in
           thread_add_message session uid message)

let thread_add_message_text = server_function ~name:"thread-add-message-text" Json.t<int * string> thread_add_message_text
let thread_add_message_image = server_function ~name:"thread-add-message-image" Json.t<int * string> thread_add_message_image

let delete_message (thread, author, timestamp) =
  protected_connected
    (fun session ->
       Lwt_log.ign_info_f "deleting message from thread %d, author %d, timestamp %Ld" thread author timestamp ;
       lwt _ = $thread(thread)<-messages %% (List.filter (fun message -> message.Object_thread.author <> author || message.Object_thread.timestamp <> timestamp)) in
       return (Some true))

let delete_message = server_function ~name:"thread-delete-message" Json.t<int * int * int64> delete_message

}}

{client{

open React
open Eliom_content.Html5
open Eliom_content.Html5.D

open Ys_react
open View_thread

let hook_pipe uid messages =
  match !Sessions.pipe with
    None -> ()
  | Some pipe ->
   ignore
      (lwt _ = Lwt_js.sleep 5.0 in
       Lwt_stream.iter
         (function
           | Live.ThreadNewMessage (thread_uid, message) when thread_uid = uid -> RListUnique.add messages message
           | _ -> ())
         pipe)


let factory circle close finalizer =

  let message =
    Raw.textarea ~a:[ a_placeholder "What do you want to say?" ] (pcdata "") in

  Ys_dom.delay_focus_textarea message ;

  let create _ =
    match Ys_dom.get_value_textarea message with
      "" -> Help.warning "Please enter a message"
    | message ->
      Authentication.if_connected ~mixpanel:("lambda-thread-create", [ "message", message ;
                                                                       "circle", Ys_uid.to_string circle ])
        (fun session -> return_unit)
  in

  let button_cancel =
    button
      ~button_type:`Button
      ~a:[ a_onclick close ]
      [ pcdata "Cancel" ]
  in

  let button_create =
    button
      ~button_type:`Button
      ~a:[ a_onclick create ]
      [ pcdata "Create" ]
  in

  div ~a:[ a_class [ "thread-factory" ]] [
    message ;

    div ~a:[ a_class [ "thread-factory-controls" ]] [
      button_cancel ;
      button_create ;
    ]
  ]

let format cohort thread format_message =

  let messages =
    RListUnique.init
      ~extract:(fun message -> (message.author.View_member.uid, message.timestamp))
      thread.messages
  in

  hook_pipe thread.uid messages ;

  let add_message =

    let state, update_state = S.create `Text in

    R.node
      (S.map
         (function
           | `Image ->
             let upload_box =
               Ys_darkroom.upload_box
                 ~square:false
                 ~max_width:260
                 (fun data finalizer ->
                    Authentication.if_connected
                      (fun session ->
                         rpc %thread_add_message_image (thread.uid, data)
                             (fun message ->
                                thread.messages <- message :: thread.messages ;
                                RListUnique.add messages message ;
                                update_state `Text ;
                                finalizer ())))
             in
             div ~a:[ a_class [ "box-section" ; "clearfix" ; "add-message" ]] [

               div ~a:[ a_class [ "right" ]] [
                 button
                   ~button_type:`Button
                   ~a:[ a_class [ "icon-pencil" ] ; a_onclick (fun _ -> update_state `Text) ]
                   []
               ] ;
               div ~a:[ a_class [ "upload-box" ]] [
                 upload_box ;
               ]
             ]

           | `Text ->
             let message_input = input ~input_type:`Text ~a:[ a_placeholder "Say something" ] () in

             let send _ =
               match Ys_dom.get_value message_input with
                 "" -> ()
               | _ as message ->
                 Authentication.if_connected ~mixpanel:("lambda-thread-add-message-text", [ "thread-uid", Ys_uid.to_string thread.uid ;
                                                                                            "message-text", message ])
                   (fun _ ->
                      rpc
                      %thread_add_message_text
                          (thread.uid, message)
                          (fun message ->
                             (* band aid *)
                             thread.messages <- message :: thread.messages ;
                             RListUnique.add messages message ;
                             Ys_dom.set_value message_input ""))
             in

             Ys_dom.register_on_return message_input send ;

             let send =
               button ~button_type:`Button  ~a:[ a_onclick send ] [ pcdata "Send" ]
             in

             div ~a:[ a_class [ "box-section" ; "clearfix" ; "add-message" ]] [

               div ~a:[ a_class [ "right" ]] [
                 button
                   ~button_type:`Button
                   ~a:[ a_class [ "icon-camera" ] ; a_onclick (fun _ -> update_state `Image) ]
                   []
               ] ;
               div ~a:[ a_class [ "right" ]] [ send ] ;
               div ~a:[ a_class [ "thread-input" ]] [
                 message_input
               ]
             ]

         )
         state)
  in

  let state, update_state = S.create `Collapsed in

  let format messages' =
    div ~a:[ a_class [ "thread-messages" ]]
      (List.fold_left (fun acc message -> format_message messages message :: acc) [] messages')
  in

  let inner =
    R.node
      (S.l2
         (fun state messages ->
            if List.length messages <= 4 then
               div ~a:[ a_class [ "thread-inner" ]] [
                 format messages
              ]
            else
              match state with
              | `Collapsed ->
                let head = Ys_list.take 3 messages in
                div ~a:[ a_class [ "thread-inner" ]] [
                  div ~a:[ a_class [ "thread-see-more" ]] [
                    Raw.a ~a:[ a_onclick (fun _ -> update_state `Full) ] [
                      pcdata "See " ;
                      pcdata (string_of_int (List.length messages - 3)) ;
                      pcdata " older messages" ]
                  ] ;
                  format head
                ]
              | `Full ->
                div ~a:[ a_class [ "thread-inner" ]] [
                  div ~a:[ a_class [ "thread-see-more" ]] [
                  Raw.a ~a:[ a_onclick (fun _ -> update_state `Collapsed) ] [ pcdata "See less" ]
                ] ;
                format messages
              ])
         state
         (RListUnique.channel messages))
  in

  div ~a:[ a_class [ "box" ; "lambda-thread" ]] [
    inner ;
    add_message
  ]

let format_regular cohort thread =

  let format_message messages message =
    let content =
      match message.content with
      | Image image ->
        span ~a:[ a_class [ "message-content" ]] [
          match image.View_image.content with
          | Object_image.File url -> img ~alt:"" ~src:(uri_of_string (fun () -> url)) ()
          | Object_image.URIData data -> img ~alt:"" ~src:(uri_of_string (fun () -> data)) ()
        ]
      | Text text ->
        let content = span [ pcdata text ] in
        Ys_autolink.link_rich content ;
        span ~a:[ a_class [ "message-content" ]] [
          content ; pcdata " - " ;
          span ~a:[ a_class [ "message-time" ]] [ Ys_timeago.format message.timestamp ]
        ]
    in
    div ~a:[ a_class [ "thread-message" ]] [
      Lambda_member.format_cohort cohort message.author ;
      content ;
    ]
  in

  format cohort thread format_message

let format_leader cohort thread =

  let format_message messages message =
    let content =
      match message.content with
      | Image image ->
        span ~a:[ a_class [ "message-content" ]] [
          match image.View_image.content with
          | Object_image.File url -> img ~alt:"" ~src:(uri_of_string (fun () -> url)) ()
          | Object_image.URIData data -> img ~alt:"" ~src:(uri_of_string (fun () -> data)) ()
        ]
      | Text text ->
        let content = span [ pcdata text ] in
        Ys_autolink.link_rich content ;

        let delete _ =
          Authentication.if_connected
            ~mixpanel:("lambda-thread-delete-message", [ "thread-uid", Ys_uid.to_string thread.uid ;
                                                         "message", text ])
            (fun _ ->
               rpc
               %delete_message
                   (thread.uid, message.author.View_member.uid, message.timestamp)
                   (fun _ -> RListUnique.remove messages (message.author.View_member.uid, message.timestamp)))
          in
          let delete =
            button
              ~button_type:`Button
                ~a:[ a_onclick delete ; a_class [ "cl";  "icon-cancel-circled" ]]
                []
            in
            span ~a:[ a_class [ "message-content" ]] [
              content ;
              pcdata " - " ;
              Ys_timeago.format message.timestamp ;
              space () ;
              delete
            ]
    in

    div ~a:[ a_class [ "thread-message" ]] [
      Lambda_member.format_cohort cohort message.author ;
      content ;
    ]
  in

  format cohort thread format_message

}}
