(*
 * Accretio is an API, a sandbox and a runtime for social playbooks
 *
 * Copyright (C) 2015 William Le Ferrand
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)


{shared{

open Lwt
open Sessions
open Ys_uid
open Vault

type state = Drafting | Proposing | Cancelled | Done

type t =
  {
    uid : uid ;
    state : state ;
    shortlink : string ;
    min_age_in_months : int ;
    max_age_in_months : int ;

    title : string ;
    description : string ;
    summary : string ;
    number_of_spots : int ;
    bookings : View_booking.t list ;
    thread : View_thread.t ;
    suggestion_member : View_member.t ;
    suggestion_raw : string ;
  }

}}

{server{

let to_state = function
  | Object_activity.Proposing -> Proposing
  | Object_activity.Cancelled -> Cancelled
  | Object_activity.Drafting -> Drafting
  | Object_activity.Done -> Done

let to_view uid =
  lwt shortlink, state, min_age_in_months, max_age_in_months, title, description, summary, number_of_spots, bookings, thread, suggestion_member, suggestion_raw =
    $activity(uid)->(shortlink, state, min_age_in_months, max_age_in_months, title, description, summary, number_of_spots, bookings, thread, suggestion_member, suggestion_raw) in
  lwt bookings = Lwt_list.map_s (fun (_, uid) -> View_booking.to_view uid) bookings in
  let state = to_state state in
  lwt thread = View_thread.to_view thread in
  lwt suggestion_member = View_member.to_view suggestion_member in
  return {
    uid ;
    shortlink ;
    state ;
    min_age_in_months ;
    max_age_in_months ;
    title ;
    description ;
    summary ;
    number_of_spots ;
    bookings ;
    thread ;
    suggestion_member ;
    suggestion_raw ;
  }

}}

{client{

open React
open Ys_react
open Eliom_content.Html5
open Eliom_content.Html5.D

let format view =
  div ~a:[ a_class [ "view-activity" ; "box" ]] [
    div ~a:[ a_class [ "shortlink" ]] [ pcdata view.shortlink ] ;
    div ~a:[ a_class [ "title" ]] [ pcdata view.title ] ;
    div ~a:[ a_class [ "description" ]] [ pcdata view.description ] ;
    div ~a:[ a_class [ "summary" ]] [ pcdata view.summary ] ;
    div ~a:[ a_class [ "number_of_spots" ]] [ pcdata (string_of_int view.number_of_spots) ] ;
    div ~a:[ a_class [ "bookings" ]] (List.map View_booking.format view.bookings)
  ]

}}
