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


(* relies on moment.js *)

open React
open Eliom_content.Html5
open Eliom_content.Html5.D

let now () =
  Int64.of_float (Js.to_float (jsnew Js.date_now ())##getTime() /. 1000.)

let to_timestamp date =
  Int64.of_int (((jsnew (Js.Unsafe.variable "moment") (Js.string date))##valueOf()) / 1000)

let format ?(a=[]) timestamp =
  let moment = (Js.Unsafe.variable "moment")##unix(Js.number_of_float (Int64.to_float timestamp)) in
  let content, update_content = S.create (moment##fromNow()) in
  let _ =
    Dom_html.window##setInterval ((Js.wrap_callback (fun () -> update_content (moment##fromNow()))), 60000.)
  in
  span ~a [
    R.node (S.map (fun s -> pcdata (Js.to_string s)) content)
  ]

let countdown ?(a=[]) timestamp =

  let content =
    if now () < timestamp then
      let content, update_content = S.create "" in
      let update () =
        let now = now () in
        if now < timestamp then
          begin
            let duration = Int64.sub timestamp now in
            let moment =
              (Js.Unsafe.variable "moment")##duration(Js.number_of_float (Int64.to_float duration *. 1000.0))
            in
            let formatted = Js.to_string (moment##humanize()) in
            update_content ("Time remaining: " ^ formatted)
          end
        else
          update_content "Pool closed"
      in
      update () ;
      ignore (Dom_html.window##setInterval (Js.wrap_callback update, 60000.)) ;
      content
    else
      S.const "Pool closed"
  in

  span ~a [
    R.node (S.map (fun s -> pcdata s) content)
  ]


let format_future ?(a=[]) timestamp =
  let moment = (Js.Unsafe.variable "moment")##unix(Js.number_of_float (Int64.to_float timestamp)) in
  let content, update_content = S.create (moment##fromNow(Js._true)) in
  let _ =
    Dom_html.window##setInterval ((Js.wrap_callback (fun () -> update_content (moment##fromNow(Js._true)))), 60000.)
  in
  span ~a [
    R.node (S.map (fun s -> pcdata (Js.to_string s)) content)
  ]

let format_static timestamp =
  let moment = (Js.Unsafe.variable "moment")##unix(Js.number_of_float (Int64.to_float timestamp)) in
  let formatted = Js.to_string (moment##format(Js.string "MMMM Do, h:mm a")) in
  span [
    pcdata formatted
  ]
