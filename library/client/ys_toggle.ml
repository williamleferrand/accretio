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


open Lwt
open React
open Eliom_content
open Html5
open D

let create factory finalizer =
  let state, update_state = S.create `Closed in
  let toggle arg =
    match S.value state with
    | `Closed -> update_state (`Open arg)
    | `Open _ -> update_state `Closed
  in
  let content =
    R.node
      (S.map
         (function
           | `Closed -> div []
           | `Open arg ->
             factory (fun _ -> update_state `Closed) (fun elt -> update_state `Closed ; finalizer elt) arg)
         state)
  in
  toggle, content


let create_with_button ?(a=[]) ?(class_open=[]) ?(label_open="Open") ?(label_close="Close") factory finalizer =
  let state, update_state = S.create `Closed in
  R.node
    (S.map
       (function
         | `Closed ->
           button
             ~button_type:`Button
             ~a:[ a_onclick (fun _ -> update_state `Open) ; a_class class_open ]
             [ pcdata label_open ]
         | `Open ->
           div ~a
             [
               (if label_close = "" then pcdata "" else
                  button
                    ~button_type:`Button
                    ~a:[ a_onclick (fun _ -> update_state `Closed) ]
                    [ pcdata label_close ]) ;
               factory (fun _ -> update_state `Closed) (fun elt -> update_state `Closed ; finalizer elt)
             ])
       state)

let flipper () =
  let state, update_state = S.create false in
  state,
  R.node
    (S.map
       (function
         | true ->
           button
             ~button_type:`Button
             ~a:[ a_onclick (fun _ -> update_state false) ]
             [ pcdata "Deselect" ]
         | false ->
           button
             ~button_type:`Button
             ~a:[ a_onclick (fun _ -> update_state true) ]
             [ pcdata "Select" ])
       state)
