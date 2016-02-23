(*
 * mandarin_circle_time
 *
 * this playbook schedules a recurring mandarin circle time for preschoolers
 *
 * william@accret.io
 *
 *)

open Lwt

open Printf
open CalendarLib
open Calendar.Precise

open Api

open Eliom_content.Html5
open Eliom_content.Html5.D

open Ys_uid
open Message_parsers
open Toolbox

let author = "william@accret.io"
let name = "Mandarin Circle Time"
let description = "This playbook schedules a recurring mandarin circle time for preschoolers"

let version = 0


(* tags ***********************************************************************)

let tag_teacher = "teacher"
let key_date = "date"
let key_location = "location"

(* some stages ****************************************************************)

let schedule_circle_time context () =
  match_lwt context.search_members ~query:tag_teacher () with
    [] ->
    return `FindTeacher
  | teacher :: _ ->
    lwt salutations = salutations context.society_supervisor in
    lwt _ =
      context.message_supervisor
        ~subject:"Schedule the next Mandarin Circle Time"
        ~content:[
          salutations ; br () ;
          br () ;
          pcdata "When is the next Mandarin Circle Time? Please give me a ISO 8601 date. (eg 2016-05-15T08:30:00)" ; br () ;
          br () ;
          pcdata "Thanks" ; br () ;
        ]
        ()
    in
    return `None

let find_teacher context () =
  lwt salutations = salutations context.society_supervisor in
  lwt _ =
    context.message_supervisor
      ~subject:"Teacher needed for the Mandarin Circle Time group"
      ~content:[
        salutations ; br () ;
        br () ;
        pcdata "A teacher is needed to schedule the circle time. Can you point me out to someone?" ; br () ;
        br () ;
        pcdata "Thanks," ; br () ;
      ]
      ()
  in
  return `None

let extract_teacher context message =
  lwt uids = extract_and_create_all_members_from_message context message in
  let uids = List.filter (fun uid -> uid <> context.society_supervisor) uids in
  match uids with
    [] ->
    lwt _ =
      context.reply_to
        ~message
        ~content:[
          pcdata "Oops, I couldn't find a valid email. Want to try again?"
        ]
        ()
    in
    return `None
  | member :: _ ->
    lwt email = $member(member)->preferred_email in
    lwt _ = context.tag_member ~member ~tags:[ tag_teacher ] in
    lwt _ =
      context.reply_to
        ~message
        ~content:[ pcdata "Thanks, I added " ; pcdata email ; pcdata " in the teacher roaster." ; br () ]
        ()
    in
    return `ScheduleCircleTime

let iso_date = "%FT%H:%M:%S"

let extract_time context message =
  lwt date = context.get_message_content ~message in
  try
    let _ = CalendarLib.Printer.Calendar.from_fstring iso_date date in
    return (`AskLocation (date, message))
  with exn ->
    context.log_error ~exn "couldn't parse date" ;
    lwt _ =
      context.reply_to
        ~message
        ~content:[ pcdata "Sorry I couldn't parse the date, can you try again?" ]
        ()
    in
    return `None

let ask_location context (date, message) =
  lwt _ =
    context.reply_to
      ~data:[ key_date, date ]
      ~message
      ~content:[ pcdata "Thanks, and where will it take place?" ]
      ()
  in
  return `None

let extract_location context message =
  match_lwt context.get_message_data ~message ~key:key_date with
    None ->
    lwt _ =
      context.reply_to
        ~message
        ~content:[
          pcdata "I couldn't find a date associated with this event"
        ]
        ()
    in
    return `None
  | Some date ->
    lwt location = context.get_message_content ~message in
    (* as a sanity check we return the location to the sender and add a delay on
       the next step so that the supervisor can chime in and correct poor email
       parsing *)
    lwt _ =
      context.reply_to
        ~message
        ~content:[
          pcdata "Great, I'll schedule the circle time on " ; pcdata date ; pcdata " at:" ; br () ;
          br () ;
          i [ pcdata location ] ; br () ;
          br () ;
          pcdata "Thanks!" ; br () ;
        ]
        ()
    in
    lwt _ =
      context.set_timer
        ~duration:(Calendar.Period.lmake ~hour:1 ())
        (`AskTeacher (date, location))
    in
    return `None

let ask_teacher context (date, location) =
  match_lwt context.search_members ~query:tag_teacher () with
    [] ->
    return `FindTeacher
  | member :: _ ->
    let calendar_date = CalendarLib.Printer.Calendar.from_fstring iso_date date in
    let pretty_date = CalendarLib.Printer.Calendar.sprint "%B %d (it's a %A), at %I:%M %p" calendar_date in
    lwt salutations = salutations member in
    lwt _ =
      context.message_member
        ~member
        ~subject:"Mandarin Circle Time"
        ~data:[ key_date, date ; key_location, location ]
        ~content:[
          salutations ; br () ;
          br () ;
          pcdata "I'm thinking of scheduling a Mandarin Circle Time on " ; pcdata pretty_date ; pcdata "." ;
          pcdata " We would meet at " ; pcdata location ; pcdata "." ; br () ;
          br () ;
          pcdata "Would you be able to lead the Circle Time?"
        ]
        ()
    in
    return `None

let teacher_is_available context message =
  lwt member = context.get_message_sender ~message in
  return `None

let teacher_is_not_available context message =
  lwt member = context.get_message_sender ~message in
  return `None

(* the flow *******************************************************************)

PLAYBOOK

schedule_circle_time<forward> ~> `Message of email ~> extract_time<forward> ~> `Message of email ~> extract_time
                                                      extract_time ~> `AskLocation of (string * int) ~> ask_location

 ask_location<forward> ~> `Message of email ~> extract_location<forward> ~> `Message of email ~> extract_location
 extract_location ~> `AskTeacher of (string * string) ~> ask_teacher ~> `FindTeacher ~> find_teacher

 ask_teacher ~> `TeacherIsAvailable of email ~> teacher_is_available
 ask_teacher ~> `TeacherIsNotAvailable of email ~> teacher_is_not_available

 schedule_circle_time ~> `FindTeacher ~> find_teacher<forward> ~> `Message of email ~> extract_teacher ~> `ScheduleCircleTime ~> schedule_circle_time

 extract_teacher<forward> ~> `Message of email ~> extract_teacher
