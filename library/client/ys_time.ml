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


open React
open Eliom_content
open Html5
open D

let format_timestamp t =
  pcdata ""

let timestamp_to_isostring timestamp =
  let t = jsnew Js.date_fromTimeValue (Int64.to_float timestamp *. 1000.) in
  Js.to_string (t##toISOString ())

let isostring_to_timestamp isostring =
  Int64.of_float (Js.date##parse(Js.string isostring) /. 1000.)

let date_picker () =
  let datetime = input ~a:[ a_input_type `Datetime_local ] () in

  let get () =
    match Ys_dom.get_value datetime with
      "" -> None
    | datetime ->
      let timestamp = Ys_timeago.to_timestamp datetime in
      Some timestamp
  in

  let dom =
    div ~a:[ a_class [ "datepicker" ]] [
      datetime ;
    ]
  in

   dom, get


let date_picker_react ?default () =
  let datetime = input
      ~a:(match default with
            None -> [ a_input_type `Datetime_local ]
          | Some timestamp -> [ a_input_type `Datetime_local ; a_value (string_of_float (Int64.to_float timestamp *. 1000.)) ]) () in

  let timestamp, update_timestamp = S.create default in

  let get () =
    match Ys_dom.get_value datetime with
      "" -> None
    | datetime ->
      let timestamp = Ys_timeago.to_timestamp datetime in
      Some timestamp
  in

  Manip.Ev.onchange
    datetime
    (fun _ ->
       update_timestamp (get ()) ;
       true) ;

  datetime, timestamp
