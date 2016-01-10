(*
 * early childhood development - field trips
 *
 * this playbook wakes up periodically and run sanity checks
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


let author = "william@accret.io"
let name = "Field trips for preschoolers"
let description = "Organizes field trips for prescholers every Wednesday"

let version = 0

(* some utilities *************************************************************)

let current_week () =
   let now = Calendar.now () in
   Calendar.year now * 53 + Calendar.week now

let next_week () =
  current_week () + 1

let key_suggestion = sprintf "suggestion-week-%d"
let key_week = "week"
let tag_already_asked = sprintf "alreadyasked%d"

(* the stages *****************************************************************)

let organize_next_field_trip context () =
  let next_week = next_week () in
  context.log_info "organizing the next field trip for week %d" next_week ;
  lwt _ =
    context.message_supervisor
      ~subject:"Please submit a suggestion for next week's field trip"
      ~content:[
        pcdata "Good morning," ; br () ;
        br () ;
        pcdata "What do you have in mind for next week's field trip?" ; br () ;
        br () ;
        pcdata "Please reply a simple description, I'll forward it directly to all members" ;
        pcdata " and collect their opinion." ; br () ;
        br () ;
        pcdata "If you don't want to organize a field trip, simply ignore this message" ; br () ;
        br () ;
        pcdata "Thanks"
      ]
      () in
  return `None

let forward_suggestion_to_all_members context message =
  lwt suggestion = context.get_message_content ~message in
  let next_week = next_week () in
  context.log_info "suggestion for week %d is %s" next_week suggestion ;
  lwt _ = context.set ~key:(key_suggestion next_week) ~value:suggestion in
  lwt members = context.search_members ~query:(sprintf "active -alreadyasked%d" next_week) () in
  context.log_info "sending the suggestion for week %d to %d members" next_week (List.length members) ;
  lwt _ =
    Lwt_list.iter_s
      (fun member ->
         lwt _ =
           context.message_member
             ~member
             ~subject:"Next week's field trip suggestion"
             ~data:[ key_week, string_of_int next_week ]
             ~content:[
               pcdata "Hi" ; br () ;
               br () ;
               pcdata suggestion ; br () ;
               br () ;
               pcdata "Are you intersted in joining?" ;
             ]
             ()
         in
         context.tag_member ~member ~tags:[ tag_already_asked next_week ])
      members
  in
  return `None

let mark_not_coming context message =
  lwt member = context.get_message_sender ~message in
  context.log_info "member %d is not coming" member ;
  return `None

let remove_member context message =
  lwt member = context.get_message_sender ~message in
  context.log_info "removing member %d definitely" member ;
  lwt _ = context.remove_member ~member in
  return `None


(* the playbook ***************************************************************)

PLAYBOOK

 *organize_next_field_trip<forward> ~> `Message of int ~> forward_suggestion_to_all_members ~> `NotComing of email ~> mark_not_coming ~> `Unsubscribe of email ~> remove_member


(* the crontab ****************************************************************)

CRON organize_next_field_trip "42 8 * * 4 *" (* initially set to trigger on the Thursday for the next Wed *)
