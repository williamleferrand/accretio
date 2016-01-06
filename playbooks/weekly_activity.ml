(* weekly_activity.ml *)

open Lwt

open Printf
open CalendarLib

open Api

open Eliom_content.Html5
open Eliom_content.Html5.D

open Message_parsers

let current_week () =
  let now = Calendar.now () in
  Calendar.year now * 53 + Calendar.week now

let pickup_weekly_organizer context () =
  let week = current_week () in
  match_lwt context.search_members ~query:(sprintf "active -declined%d -asked%d" week week) () with
    [] ->
    context.log_info "no participants for week %d" week ;
    return `NoOrganizerAvailable
  | _ as participants ->
    let member = List.nth participants (Random.int (List.length participants)) in
    context.log_info
      "picking up organizer for week %d, there are %d participants, asking %d"
      week
      (List.length participants)
      member ;
    return (`Candidate member)

let candidate_was_notified context member =
  let week = current_week () in
  context.log_info "member %d was notified, setting up a timer" member ;
  lwt _ =
    context.set_timer
      ~label:(sprintf "waiting%05d%d" week member)
      ~duration:(Calendar.Period.lmake ~second:30 ())
      `CandidateDidntReplied
  in
  lwt _ = context.tag_member ~member ~tags:[ sprintf "asked%d" week ] in
  return `None


let candidate_declined context message =
  return `PickUpSomeoneElse

let candidate_accepted context message =
  lwt member = context.get_message_sender ~message in
  return (`Organizer member)


COMPONENT

  #import basics

  pickup_weekly_organizer ~> `NoOrganizerAvailable ~> alert_supervisor

                              candidate_was_notified ~> `CandidateDidntReplied ~> pickup_weekly_organizer
                              candidate_declined ~> `PickUpSomeoneElse ~> pickup_weekly_organizer
                              candidate_accepted
