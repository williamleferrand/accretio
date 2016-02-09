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
open Toolbox

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
let tag_pilot = "pilot"

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
        pcdata "Great! Have you been in a small plane before?"
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

let retag_members context () =
  (* this is a silly stage .. *)
  lwt members = context.search_members ~query:"active" () in
  lwt _ =
    Lwt_list.iter_s
      (fun member ->
         match_lwt context.get ~key:(key_skills member) with
           Some "pilot" ->
           lwt _ = context.tag_member ~member ~tags:[ tag_pilot ] in
           return_unit
         | _ -> return_unit)
      members in
  return `None

let schedule_event context () =
  lwt pilots = context.search_members ~query:tag_pilot () in
  lwt non_pilots = context.search_members ~query:(sprintf "active -%s" tag_pilot) () in
  lwt pilots = ul_of_members context pilots in
  lwt non_pilots = ul_of_members context non_pilots in
  let run_id = new_run_id () in
  lwt _ =
    context.message_supervisor
      ~subject:"Scheduling a flying club event"
      ~data:(data_run_id run_id)
      ~content:[
        pcdata "Greetings," ; br () ;
        br () ;
        pcdata "Here is the current members of the group, split in pilots and non pilots." ; br () ;
        br () ;
        pcdata "Pilots:" ; br () ;
        pilots ;
        br () ;
        pcdata "Non-pilots:" ; br () ;
        non_pilots ;
        br () ;
        pcdata "What would you like to suggest to pilots?" ; br () ;
      ]
      ()
  in
  return `None

let key_suggestion = sprintf "suggestion-%Ld"
let tag_already_suggested = sprintf "suggested%Ld"
let timer_remind_pilot = sprintf "remindpilot%Ld"

let suggest_to_pilots context run_id =
  match_lwt context.get ~key:(key_suggestion run_id) with
    None -> return `None
  | Some suggestion ->
    lwt pilots = context.search_members ~query:(sprintf "%s -%s" tag_pilot (tag_already_suggested run_id)) () in
    lwt _ =
      Lwt_list.iter_s
        (fun member ->
           match_lwt context.check_tag_member ~member ~tag:(tag_already_suggested run_id) with
             true -> return_unit
           | false ->
             match_lwt
               context.message_member
                 ~member
                 ~data:(data_run_id run_id)
                 ~subject:"Next flying+dinner event"
                 ~content:[
                   pcdata "Greetings," ;
                   br () ;
                   pcdata suggestion ; br () ;
                   br () ;
                   pcdata "Would you be interested in joining? No commitment yet, I'm just trying to get a sense of who could come." ; br () ;
                   br () ;
                   pcdata "Let me know!" ;
                   br () ;
                 ]
                 ()
             with
             | None -> return_unit
             | Some message ->
               lwt _ =
                 context.set_timer
                   ~label:(timer_remind_pilot run_id)
                   ~duration:(Calendar.Period.lmake ~day:1 ())
                   (`RemindPilot (run_id, message))
               in
               lwt _ = context.tag_member ~member ~tags:[ tag_already_suggested run_id ] in
               return_unit)
        pilots
    in
    lwt _ =
      context.set_timer
        ~duration:(Calendar.Period.lmake ~day:3 ())
        (`SummarizePilotsInterest run_id)
    in
    return `None

let remind_pilot context (run_id, message) =
  lwt _ =
    context.reply_to
      ~message
      ~data:(data_run_id run_id)
      ~content:[ pcdata "Sorry for the reminder, but I'm wrapping the headcount - are you interested in joining? (No commitment yet, just trying to feel the waters)" ]
      ()
  in
  return `None

let suggest_to_pilots context message =
  match_lwt run_id_from_message context message with
    None -> return `None
  | Some run_id ->
    match_lwt context.get ~key:(key_suggestion run_id) with
      Some _ ->
      suggest_to_pilots context run_id
    | None ->
      lwt suggestion = context.get_message_content ~message in
      lwt _ = context.set ~key:(key_suggestion run_id) ~value:suggestion in
      suggest_to_pilots context run_id

let tag_interested = sprintf "interested%Ld"
let tag_not_interested = sprintf "notinterested%Ld"

let mark_interested context message =
  match_lwt run_id_from_message context message with
  | None ->
    lwt _ =
      context.forward_to_supervisor ~message ~subject:"Who sent this?" ~content:[ pcdata "Can't find run_id" ] ()
    in
    return `None
  | Some run_id ->
    lwt _ = context.cancel_timers ~query:(timer_remind_pilot run_id) in
    lwt member = context.get_message_sender ~message in
    lwt _ = context.tag_member ~member ~tags:[ tag_interested run_id ] in
    lwt _ = context.untag_member ~member ~tags:[ tag_not_interested run_id ] in
    lwt _ =
      context.reply_to
        ~message
        ~content:[ pcdata "Thanks, I'll keep you posted!" ]
        ()
    in
    return `None

let mark_not_interested context message =
  match_lwt run_id_from_message context message with
  | None ->
    lwt _ =
      context.forward_to_supervisor ~message ~subject:"Who sent this?" ~content:[ pcdata "Can't find run_id" ] ()
    in
    return `None
  | Some run_id ->
    lwt _ = context.cancel_timers ~query:(timer_remind_pilot run_id) in
    lwt member = context.get_message_sender ~message in
    lwt _ = context.tag_member ~member ~tags:[ tag_not_interested run_id ] in
    lwt _ = context.untag_member ~member ~tags:[ tag_interested run_id ] in
    lwt _ =
      context.reply_to
        ~message
        ~content:[ pcdata "Ok, I'll keep you posted about future events" ]
        ()
    in
    return `None

let remove_from_group context message =
  lwt _ =
    match_lwt run_id_from_message context message with
      None -> return_unit
    | Some run_id ->
      lwt _ = context.cancel_timers ~query:(timer_remind_pilot run_id) in
      return_unit in
  lwt member = context.get_message_sender ~message in
  lwt _ = context.remove_member ~member in
  return `None

let summarize_pilots_interest context run_id =
  lwt pilots = context.search_members ~query:(tag_interested run_id) () in
  lwt suggestion =
    match_lwt context.get ~key:(key_suggestion run_id) with
      None -> return ""
    | Some suggestion -> return suggestion
  in
  match pilots with
    [] ->
    lwt _ =
      context.message_supervisor
        ~subject:"Summary of pilot's interest"
        ~data:(data_run_id run_id)
        ~content:[
          pcdata "Greetings," ; br () ;
          br () ;
          pcdata "Unfortunately, nobody expressed interest for your suggestion "; i [ pcdata suggestion ] ; br () ;
          br () ;
        ]
        ()
    in
    return `None
  | _ as members ->

    lwt members = ul_of_members context members in
    lwt _ =
      context.message_supervisor
        ~subject:"Summary of pilots' interest"
        ~data:(data_run_id run_id)
      ~content:[
        pcdata "Greetings," ; br () ;
        br () ;
        pcdata "So far the following pilots expressed interest for your suggestion "; i [ pcdata suggestion ] ; pcdata ":" ; br () ;
        br () ;
        members
      ]
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

*retag_members

*schedule_event<forward> ~> `Message of email ~> suggest_to_pilots ~> `Interested of email ~> mark_interested
                                                 suggest_to_pilots ~> `NotInterested of email ~> mark_interested
                                                 suggest_to_pilots ~> `RemoveFromGroup of email ~> remove_from_group

        suggest_to_pilots ~> `SummarizePilotsInterest of int64 ~> summarize_pilots_interest
        suggest_to_pilots ~> `RemindPilot of (int64 * int) ~> remind_pilot

CRON collect_experience "0 0 * * * *"
