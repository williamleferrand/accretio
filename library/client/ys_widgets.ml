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
open Eliom_content.Html5
open Eliom_content.Html5.D
open Ys_react

(* let's put some factored code in there .. *)

(* this widget displays a list of elements that can be created from a string *)

let appender extract init format builder =
  let elements = RListUnique.init ~extract init in

  let input = input ~input_type:`Text () in
  let add _ =
    match Ys_dom.get_value input with
      "" -> ()
    | _ as content ->
      builder
        content
        (fun result ->
           Ys_dom.set_value input "" ;
           RListUnique.add elements result)
  in

  Ys_dom.register_on_return input add ;

  [
    RListUnique.map_in_div format elements ;
    div ~a:[ a_class [ "ys-widgets-appender" ; "clearfix" ]] [
      input ;
      button
        ~button_type:`Button
        ~a:[ a_onclick add ]
        [ pcdata "Add" ]
    ]
  ]
