(*
 * core - join request
 *
 * william@accret.io
 *
 *)

open Lwt

open Printf
open CalendarLib

open Api

open Eliom_content.Html5
open Eliom_content.Html5.D

open Message_parsers

let key_original_message = "original-message"
let key_default_welcome_message = "default-welcome-message"
let tag_timer_custom_welcome_message = sprintf "timercustomwelcomemessage%d"
let key_number_of_rejections = sprintf "number-of-rejections-%d"

let process_join_request context message =
  lwt member = context.get_message_sender ~message in
  lwt content = context.get_message_content ~message in
  context.log_info "processing a join request from member %d" member ;
  lwt preferred_email = $member(member)->preferred_email in
  lwt _ =
    context.forward_to_supervisor
      ~message
      ~data:[ key_original_message, string_of_int message ]
      ~subject:("New join request in society " ^ context.society_name)
      ~content:[
        pcdata "Hi," ; br () ;
        br () ;
        pcdata "You have a new join request from ";
        pcdata preferred_email ;
        pcdata ". " ;
        pcdata "Do you want to add this member to the group? (just reply 'yes' or 'no')" ; br () ;
      ]
      ()
  in
  return `None

let add_member context message =
  match_lwt context.get_message_data ~message ~key:key_original_message with
    None ->
    lwt _ =
      context.reply_to
        ~message
        ~content:[
          pcdata "Help, can't locate the applicant .."
        ]
        ()
    in
    return `None
  | Some original_message ->
    let original_message = int_of_string original_message in
    lwt member = context.get_message_sender ~message:original_message in
    lwt _ = context.add_member ~member in
    lwt _ =
      context.reply_to
        ~message
        ~data:[ key_original_message, string_of_int original_message ]
        ~content:[
          pcdata "Great, do you want to welcome this new member personnally? If you do, just reply to this message with the custom message. If you don't reply to this message within 12 hours, we'll send the default welcome message"
        ]
        ()
    in
    lwt _ =
      context.set_timer
        ~label:(tag_timer_custom_welcome_message member)
        ~duration:(Calendar.Period.lmake ~hour:12 ())
        (`SendDefaultWelcomeMessage original_message)
    in
    return `None

let send_custom_welcome_message context message =
  lwt welcome_message = context.get_message_content ~message in
  match_lwt context.get_message_data ~message ~key:key_original_message with
    None -> return `None
  | Some original_message ->
    let original_message = int_of_string original_message in
    lwt member = context.get_message_sender ~message:original_message in
    lwt _ = context.cancel_timers ~query:(tag_timer_custom_welcome_message member) in
    lwt _ =
      context.reply_to
        ~message:original_message
        ~content:[ pcdata welcome_message ]
        ()
    in
    lwt _ =
      context.reply_to
        ~message
        ~content:[ pcdata "Done!" ]
        ()
    in
    return `None

let send_default_welcome_message context message =
  match_lwt context.get ~key:key_default_welcome_message with
  | None ->
    context.log_warning "there is no default welcome message" ;
    lwt _ =
      context.message_supervisor
        ~subject:"Do you want to set up a default welcome message?"
        ~data:[ key_original_message, string_of_int message ]
        ~content:[
          pcdata "Hi," ; br () ;
          br () ;
          pcdata "Just reply above the message with a default welcome message if you want to set up one. It will be sent to new members if you don't specify a custom welcome message within 12 hours of approving them in"; br () ;
        ]
        ()
    in
    return `None
  | Some welcome_message ->
    lwt _ =
      context.reply_to
        ~message
        ~content:[ pcdata welcome_message ]
        ()
    in
    (* cleanup all the timers, just in case we came to that stage after setting
       up the default message *)
    lwt member = context.get_message_sender ~message in
    lwt _ = context.cancel_timers ~query:(tag_timer_custom_welcome_message member) in
    return `None

let setup_default_welcome_message context message =
  lwt welcome_message = context.get_message_content ~message in
  lwt _ = context.set ~key:key_default_welcome_message ~value:welcome_message in
  lwt _ =
    context.reply_to
      ~message
      ~content:[
        pcdata "Thanks, the default message has been updated with: " ; br () ;
        br () ;
        i [ pcdata welcome_message ] ; br () ;
        br () ;
        pcdata "Reply above this message if you want to update it"
      ]
      ()
  in
  (* if there is an original message attached to this one then we setup a timer.
     we don't want to send the welcome message immediately, just in case the
     leader made a mistake & wants to amend the message. let's give him 1h *)
  match_lwt context.get_message_data ~message ~key:key_original_message with
    None -> return `None
  | Some original_message ->
    let original_message = int_of_string original_message in
    lwt member = context.get_message_sender ~message:original_message in
    lwt _ =
      context.set_timer
        ~label:(tag_timer_custom_welcome_message member)
        ~duration:(Calendar.Period.lmake ~hour:1 ())
        (`SendDefaultWelcomeMessage original_message)
    in
    return `None

let reject_member context message =
  match_lwt context.get_message_data ~message ~key:key_original_message with
    None -> return `None
  | Some original_message ->
    let original_message = int_of_string original_message in
    lwt member = context.get_message_sender ~message:original_message in
    context.log_info "rejecting member %d" member ;
    lwt _ =
      context.reply_to
        ~message:original_message
        ~content:[
          pcdata "Sorry, but it seems like this group doesn't accept new members right now. Feel free to re-use the playbook to start your own!" ; br ()
        ]
        ()
    in
    lwt _ =
      context.reply_to
        ~message
        ~content:[
          pcdata "Rejection message sent"
        ]
        ()
    in
    lwt number_of_rejections =
      match_lwt context.get ~key:(key_number_of_rejections member) with
        None -> return 0
      | Some nb -> return (int_of_string nb)
    in
    lwt _ = context.set ~key:(key_number_of_rejections member) ~value:(string_of_int (number_of_rejections + 1)) in
    return `None

COMPONENT

-process_join_request<simple_yes_no> ~> `No of email ~> reject_member
-process_join_request ~> `Yes of email ~> add_member<forward> ~> `Message of email ~> send_custom_welcome_message
                                         add_member ~> `SendDefaultWelcomeMessage of int ~> send_default_welcome_message<forward> ~> `Message of email ~> setup_default_welcome_message<forward> ~> `Message of email ~> setup_default_welcome_message
                                                                                                                                                                     setup_default_welcome_message ~> `SendDefaultWelcomeMessage of int ~> send_default_welcome_message
