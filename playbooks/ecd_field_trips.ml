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
open Toolbox

let author = "william@accret.io"
let name = "Field trips for preschoolers"
let description = "Organizes field trips for prescholers every Wednesday"

let version = 0

(* some utilities *************************************************************)

let tag_already_asked = sprintf "alreadyasked%Ld"
let tag_organizer = "organizer"
let tag_volunteer = sprintf "volunteer%Ld"

(* the stages *****************************************************************)

(* PART 1 - looking up an organizer. we use a specific tag, tag_organizer, to
   mark parents that can make suggestions *)

let organize_field_trip context () =
  let run_id = Ys_time.now () in
  return (`FindOrganizer run_id)

let find_organizer context run_id =
  match_lwt context.search_members ~query:(tag_organizer ^ " -" ^ (tag_already_asked run_id)) () with
    [] ->
    lwt _ =
      context.message_supervisor
        ~subject:"No registered supervisors"
        ~content:[
          pcdata "There is no available registered organizer, you'll have to be the one making a suggestion! What do you have in mind, and when?"
        ]
        ()
    in
    return `None
  | organizers ->
    let member = List.nth organizers (List.length organizers) in
    lwt actives = context.search_members ~query:"active" () in
    context.log_info "picking up organizer %d" member ;
    lwt _ =
      context.message_member
        ~member
        ~data:(data_run_id run_id)
        ~subject:"Suggesting a field trip?"
        ~content:[
          pcdata "Greetings," ; br () ;
          br () ;
          pcdata "Would you like to suggest a field trip to the ";
          pcdata (string_of_int (List.length actives)) ;
          pcdata " members of the group?"
        ]
        ()
    in
    return `None

let ask_someone_else context message =
  match_lwt run_id_from_message context message with
    None -> return `None
  | Some run_id ->
    lwt member = context.get_message_sender ~message in
    lwt _ =
      context.reply_to
        ~message
        ~content:[
          pcdata "No problem, I'll ask someone else :)"
        ]
        ()
    in
    lwt _ = context.tag_member ~member ~tags:[ tag_already_asked run_id ] in
    return (`FindOrganizer run_id)

(* PART 2 - work out a suggestion. so far it is a very manual process *********)

let mark_sender_as_volunteer context message run_id =
  lwt member = context.get_message_sender ~message in
  lwt _ = context.tag_member ~member ~tags:[ tag_volunteer run_id ] in
  return_unit

let ask_supervisor_to_summarize context message =
  match_lwt run_id_from_message context message with
    None -> return `None
  | Some run_id ->
    lwt _ = mark_sender_as_volunteer context message run_id in
    lwt _ =
      context.forward_to_supervisor
        ~message
        ~data:(data_run_id run_id)
        ~subject:"Summarize proposal"
        ~content:[
          pcdata "Can you summarize the proposal above?"
        ]
        ()
    in
    return `None

let ask_organizer_for_details context message =
  match_lwt run_id_from_message context message with
    None -> return `None
  | Some run_id ->
    lwt _ = mark_sender_as_volunteer context message run_id in
    lwt _ =
      context.reply_to
        ~message
        ~content:[
          pcdata "Wonderful! What would you like to suggest? Let's start by an activity and a date, and let's see what the others think!" ;
        ]
        ()
    in
    return `None

(* the flow *******************************************************************)

PLAYBOOK

*organize_field_trip ~> `FindOrganizer of int64 ~> find_organizer

     find_organizer ~> `No of email ~> ask_someone_else ~> `FindOrganizer of int64 ~> find_organizer
     find_organizer ~> `YesWithDetails of email ~> ask_supervisor_to_summarize
     find_organizer ~> `Yes of email ~> ask_organizer_for_details<forward> ~> `Message of email ~> ask_supervisor_to_summarize
