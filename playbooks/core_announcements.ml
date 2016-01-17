(*
 * core - annoucements
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

(* the tags *)

let key_content = sprintf "core-announcement-content-%Ld"
let key_id = "core-announcement-id"
let tag_timer = sprintf "coreannouncementtimer%Ld%d"

(* the stages *)

let make_announcement context (content, members) =
  context.log_info "making announcement to %d members" (List.length members) ;
  let id = Ys_time.now () in
  lwt _ = context.set ~key:(key_content id) ~value:content in
  lwt _ =
    Lwt_list.iter_s
      (fun member ->
         lwt _ =
           context.message_member
             ~member
             ~data:[ key_id, Int64.to_string id ]
             ~subject:(sprintf "IMPORTANT message regarding '%s'" context.society_name)
             ~content:[
               pcdata "Hi," ; br () ;
               br () ;
               pcdata content ; br () ;
               br () ;
               pcdata "Please drop me a line once you see this message so that I can be sure I didn't forgot anybody :)"; br () ;
               br () ;
               pcdata "Thanks"
             ]
             ()
         in
         lwt _ =
           context.set_timer
             ~label:(tag_timer id member)
             ~duration:(Calendar.Period.lmake ~hour:6 ())
             (`RemindMember (id, member, 0))
         in
         return_unit)
      members
  in
  return `None

let mark_as_read context message =
  match_lwt context.get_message_data ~message ~key:key_id with
    None -> return `None
  | Some id ->
    let id = Int64.of_string id in
    lwt member = context.get_message_sender ~message in
    lwt _ = context.cancel_timers ~query:(tag_timer id member) in
    return `None

let remind_member context (id, member, attempts) =
  context.log_info "restarting the remind_member for run %Ld" id ;
  match_lwt context.get ~key:(key_content id) with
    None -> return `None
  | Some content ->
    if attempts > 2 then
      lwt email = $member(member)->preferred_email in
      lwt _ =
        context.message_supervisor
          ~subject:"Unresponsive member"
          ~content:[
            pcdata "Member "; pcdata email ; pcdata " never responded to the email below. (I did 3 attempts)"; br () ;
            br () ;
            i [ pcdata content ] ; br ()
          ]
          ()
      in
      return `None
    else
      lwt _ =
        context.message_member
          ~data:[ key_id, Int64.to_string id ]
          ~member
          ~subject:(sprintf "IMPORTANT message regarding '%s' (reminder)" context.society_name)
          ~content:[
            pcdata "Hi," ; br () ;
            br () ;
            pcdata "Sorry for resending but I would like to make sure that you have seen the message below:" ; br () ;
            br ();
            i [ pcdata content ] ; br () ;
            br () ;
            pcdata "Please drop me a line once you see this message so that I can be sure I didn't forgot anybody :)"; br () ;
            br () ;
            pcdata "Thanks"
          ]
          ()
      in
      lwt _ =
        context.set_timer
          ~label:(tag_timer id member)
          ~duration:(Calendar.Period.lmake ~hour:6 ())
          (`RemindMember (id, member, attempts + 1))
      in
      return `None

(* the plumbing *)

COMPONENT

  make_announcement<forward> ~> `Message of email ~> mark_as_read
  make_announcement ~> `RemindMember of int64 * int * int ~> remind_member ~> `RemindMember of int64 * int * int ~> remind_member
