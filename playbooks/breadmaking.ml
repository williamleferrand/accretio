open Lwt

open CalendarLib

open Api

open Eliom_content.Html5
open Eliom_content.Html5.D

open Message_parsers

let author = "william@accret.io"
let name = "Breadmaking circle"
let description = "Baking bread for the neighbors"

let version = 0

let current_week () =
   let now = Calendar.now () in
   Calendar.year now * 53 + Calendar.week now

(* the individual stages ****************************************************)

let check_if_baker_is_available_this_week context () =
  match_lwt context.search_members "active -baker" () with
    [] ->
    context.log_info "no participants this week, no need to notify the baker" ;
    return `None
  | _ as participants ->
    context.log_info "found %d participants, notifying the baker" (List.length participants) ;
    lwt _ =
      context.message_supervisor
      ~subject:"Are you available this week?"
      ~content:[
        pcdata "Hi," ; br () ;
        br () ;
        pcdata "Are you available to bake bread this week?"
      ]
      () in
  return `None

let mark_baker_as_unavailable context message =
  lwt member = context.get_original_message ~message in
  lwt member = context.get_message_sender ~message in
  let week = current_week () in
  lwt _ = context.tag_member ~member ~tags:[ Printf.sprintf "unavailable_%d" week ] in
  lwt _ = context.untag_member ~member ~tags:[ Printf.sprintf "available_%d" week ] in
  return `None

let mark_baker_as_available context message =
  lwt member = context.get_original_message ~message in
  lwt member = context.get_message_sender ~message in
  let week = current_week () in
  lwt _ = context.tag_member ~member ~tags:[ Printf.sprintf "available_%d" week ] in
  lwt _ = context.untag_member ~member ~tags:[ Printf.sprintf "unavailable_%d" week ] in
  return `AskBakerForCustomMessage

let ask_baker_for_custom_message context () =
  lwt _ =
    context.message_supervisor
      ~subject:"Please customize the weekly message"
      ~content:[
        pcdata "Hi," ; br () ;
        br () ;
        pcdata "Glad to hear that you can bake. What custom message do you want to send to your followers?"
      ]
      () in
  return `None

let send_message_to_participants context message =
  lwt content = context.get_message_content ~message in
  lwt members = context.search_members "active -baker" () in
  context.log_info "sending message to %d participants" (List.length members) ;
  match members with
    [] -> return `TellBakerThereAreNoParticipants
  | _ as member ->
  lwt _ =
    Lwt_list.iter_p
      (fun member ->
         lwt _ =
           context.message_member
             ~member
             ~subject:"Interested in some bread?"
             ~content:[
               pcdata "Hi," ; br () ;
               br () ;
               pcdata content ;
               br () ;
               pcdata "Cheers," ; br () ;
               pcdata "William"
             ]
             ()
         in return_unit)
      members
  in
  return `None

let mark_participant tags_add tags_remove context message =
  lwt original = context.get_original_message ~message in
  context.log_info "message: %d, original message is %d" message original ;
  lwt member = context.get_message_sender ~message:original in
  let week = current_week () in
  lwt _ = context.tag_member ~member ~tags:[ Printf.sprintf "%s_%d" tags_add week ] in
  lwt _ = context.untag_member ~member ~tags:[ Printf.sprintf "%s_%d" tags_remove week ] in
  return `None

let mark_participant_as_not_joining context message = mark_participant "not_joining" "joining" context message

let mark_participant_as_joining context message = mark_participant "joining" "not_joining" context message

let create_dashboard_this_week context () =
  let week = current_week () in

  lwt participants = context.search_members ~query:(Printf.sprintf "joining_%d" week) () in
  context.log_info "create dashboard for week %d, we have %d participants" week (List.length participants) ;

  lwt emails = Lwt_list.map_p (fun member -> $member(member)->preferred_email) participants in

  lwt bakers = context.search_members ~query:(Printf.sprintf "available_%d" week) () in

  match participants, bakers with
    [], [] -> return `None
  | _, _ ->
    lwt _ =
      context.message_supervisor
        ~subject:"Bread order"
        ~content:[
          pcdata "Hi," ; br () ;
          br () ;
          pcdata "There are " ; pcdata (string_of_int (List.length participants)) ; pcdata " people interested in bread this week: " ; br () ;
          br () ;
          ul (List.map (fun email -> li [ pcdata email ]) emails) ;
          br () ;
          pcdata "A vos fourneaux!" ;
        ]
    () in
  return `None

let tell_baker_there_are_no_participants context _ =
  lwt _ =
    context.message_supervisor
      ~subject:"No need for bread this week"
      ~content:[
        pcdata "Hi," ; br () ;
        br () ;
        pcdata "Actually there are no participants this week. Invite more people to the society!"; br () ;
      ]
      () in
  return `None


(* the graph ****************************************************************)

PLAYBOOK

  check_if_baker_is_available_this_week<simple_yes_no> ~> `No of email ~> mark_baker_as_unavailable
  check_if_baker_is_available_this_week<simple_yes_no> ~> `Yes of email ~> mark_baker_as_available


  mark_baker_as_available ~> `AskBakerForCustomMessage ~> ask_baker_for_custom_message

  ask_baker_for_custom_message<forward> ~> `Message of email ~> send_message_to_participants<simple_yes_no> ~> `TellBakerThereAreNoParticipants ~> tell_baker_there_are_no_participants
                                                       send_message_to_participants<simple_yes_no> ~> `Yes of email ~> mark_participant_as_joining
                                                       send_message_to_participants<simple_yes_no> ~> `No of email ~> mark_participant_as_not_joining




CRON check_if_baker_is_available_this_week "0 0 * * 1 *"
CRON create_dashboard_this_week "0 0 * * 4 *"
