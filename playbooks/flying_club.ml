(*
 * flying club
 *
 * this playbook connects pilots and non pilots to organize sightseeing trips
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

let _ =
  CalendarLib.Time_Zone.change (CalendarLib.Time_Zone.UTC_Plus (-8))

let author = "william@accret.io"
let name = "Flying club"
let description = "This playbook connects pilots and non pilots to schedule sightseeing trips"

let version = 0

(* some tags *****************************************************************)

let key_homebase = sprintf "homebase-%d"
let key_skills = sprintf "skills-%d"
let key_ratings = sprintf "ratings-%d"
let key_experience = sprintf "experience-%d"
let key_additional_seats = sprintf "additional-seats-%d"

let key_original_email = "original-email"
let tag_already_asked_for_experience = "alreadyaskedforexperience"

(* the stages *****************************************************************)

let collect_experience context () =
  lwt participants = context.search_members (sprintf "active -%s" tag_already_asked_for_experience) () in
  lwt participants =
    Lwt_list.filter_p
      (fun member ->
         match_lwt context.get ~key:(key_skills member) with
           None -> return_true
         | Some _ -> return_false)
      participants
  in
  context.log_info "collecting experience from %d members" (List.length participants) ;
  lwt _ =
    Lwt_list.map_s
      (fun member ->
         match_lwt
           context.message_member
             ~member
             ~subject:"Quick question about our next flight+dinner"
             ~content:[
               pcdata "Greetings," ; br () ;
               br () ;
               pcdata "I'm starting to plan for the next flight+dinner trip and I was wondering: where do you live? (An airport identifier or a city is enough)" ; br () ;
               br () ;
               pcdata "Thanks!"
             ]
             () with
         | None -> return_unit
         | Some message ->
           lwt _ = context.tag_member ~member ~tags:[ tag_already_asked_for_experience ] in
           return_unit)
      participants
  in

  return `None


let ask_supervisor_for_homebase context message =
  lwt _ =
    context.forward_to_supervisor
      ~message
      ~data:[ key_original_email, Ys_uid.to_string message ]
      ~subject:"Please extract homebase"
      ~content:[
        pcdata "Do you mind extracting the homebase info from the message below? Say 'ask again' if you want me to ask the member one more time" ; br () ;
        br () ;
        pcdata "Thanks"
      ]
      ()
  in
  return `None

let set_homebase context message =
  lwt content = context.get_message_content ~message in
  match_lwt context.get_message_data ~message ~key:key_original_email with
    None ->
    lwt _ =
      context.reply_to
        ~message
        ~content:[ pcdata "Can't find the original message" ]
        ()
    in
    return `None
  | Some message ->
    let message = Ys_uid.of_string message in
    lwt member = context.get_message_sender ~message in
    lwt _ = context.set ~key:(key_homebase member) ~value:content in
    match_lwt context.get ~key:(key_skills member) with
      Some _ ->
      lwt _ =
        context.reply_to
          ~message
          ~content:[ pcdata "Thanks!" ]
          ()
      in
      return `None
    | None ->
      return (`AskMemberForSkills message)

let ask_member_for_skills context message =
  lwt _ =
    context.reply_to
      ~message
      ~content:[ pcdata "Thanks! Also, are you a pilot or do you want to ride as a passenger?" ]
      ()
  in
  return `None

let set_pilot context message =
  lwt member = context.get_message_sender ~message in
  context.log_info "%d is a pilot" member ;
  lwt _ = context.set ~key:(key_skills member) ~value:"pilot" in
  lwt _ =
    context.reply_to
      ~message
      ~content:[ pcdata "Thanks!" ]
      ()
  in
  return `None

let ask_pilot_details context message =
  lwt member = context.get_message_sender ~message in
  context.log_info "%d is a pilot" member ;
  lwt _ = context.set ~key:(key_skills member) ~value:"pilot" in
  lwt _ =
    context.reply_to
      ~message
      ~content:[
        pcdata "Great! Could you briefly summarize your experience / currency?"
      ]
      ()
  in
  return `None

let ask_non_pilot_details context message =
  lwt member = context.get_message_sender ~message in
  context.log_info "%d is a non pilot" member ;
  lwt _ = context.set ~key:(key_skills member) ~value:"non-pilot" in
  lwt _ =
    context.reply_to
      ~message
      ~content:[
        pcdata "Great! Have you been in a small place before?"
      ]
      ()
  in
  return `None

let passenger_not_experienced context message =
  lwt member = context.get_message_sender ~message in
  context.log_info "%d is a non experienced passenger" member ;
  lwt _ = context.set ~key:(key_experience member) ~value:"not-experienced" in
  lwt _ =
    context.reply_to
      ~message
      ~content:[
        pcdata "Thanks! You'll have a blast!"
      ]
      ()
  in
  return `None

let passenger_experienced context message =
  lwt member = context.get_message_sender ~message in
  context.log_info "%d is an experienced passenger" member ;
  lwt _ = context.set ~key:(key_experience member) ~value:"experienced" in
  lwt _ =
    context.reply_to
      ~message
      ~content:[
        pcdata "Thanks! Be careful, you may catch the aviation bug :)"
      ]
      ()
  in
  return `None

let ask_supervisor_for_ratings context message =
  lwt member = context.get_message_sender ~message in
  lwt _ =
    context.forward_to_supervisor
      ~message
      ~data:[ key_original_email, Ys_uid.to_string message ]
      ~subject:"Please extract the ratings"
      ~content:[
        pcdata "Can you extract the ratings/currency from below? Let's do PPL/CPL & use +IR. We'll figure out currency later"
      ]
      ()
  in
  return `None

let store_ratings context message =
  lwt content = context.get_message_content ~message in
  match_lwt context.get_message_data ~message ~key:key_original_email with
    None ->
    lwt _ =
      context.reply_to
        ~message
        ~content:[ pcdata "Can't find the original member" ]
        ()
    in
    return `None
  | Some message ->
    let message = Ys_uid.of_string message in
    lwt member = context.get_message_sender ~message in
    lwt _ = context.set ~key:(key_ratings member) ~value:content in
    lwt _ =
      context.reply_to
        ~message
        ~content:[ pcdata "Thanks, looking great. Let me wait for the others' replies and I'll figure out something!" ]
        ()
    in
    return `None

(* the playbook ***************************************************************)

PLAYBOOK

   #import core_join_request
   #import core_invite


                                                                                                                                                                        ask_member_for_skills ~> `Pilot of email ~> set_pilot
   *collect_experience<forward> ~> `Message of email ~> ask_supervisor_for_homebase<forward> ~> `Message of email ~> set_homebase ~> `AskMemberForSkills of int ~> ask_member_for_skills ~> `PilotAndAskDetails of email ~> ask_pilot_details
                                                                                                                                                                   ask_member_for_skills ~> `NonPilot of email ~> ask_non_pilot_details



 ask_pilot_details<forward> ~> `Message of email ~> ask_supervisor_for_ratings<forward> ~> `Message of email ~> store_ratings

 ask_non_pilot_details ~> `NotExperiencedPassenger of email ~> passenger_not_experienced
 ask_non_pilot_details ~> `ExperiencedPassenger of email ~> passenger_experienced


CRON collect_experience "0 0 * * * *"
