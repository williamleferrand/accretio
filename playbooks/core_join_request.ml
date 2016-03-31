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

let process_join_request context () =
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

*process_join_request ~> `No of email ~> reject_member
