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


COMPONENT

  #import basics

  pickup_weekly_organizer ~> `NoOrganizerAvailable ~> alert_supervisor
