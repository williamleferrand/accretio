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


open Js
open Eliom_content.Html5
open D

let zeroclipboard () =
  Js.Unsafe.variable "ZeroClipboard"

let create () =
  jsnew (Js.Unsafe.variable "ZeroClipboard") ()

let register client content : unit =
  match Js.to_bool (client##isFlashUnusable) with
    true -> ()
  | false ->
    let button = To_dom.of_button content in
    client##clip(button) ;
    button##style##display <- string "inline" ;
    ()

let register button : unit =
  let button = To_dom.of_button button in
 let client = jsnew (zeroclipboard ()) (button) in
  client##on(string "ready", wrap_callback (fun _ ->
           Ys_log.info "zeroclipboard ready" ;
      button##style##display <- string "inline" ;

      (* client##on(string "aftercopy", wrap_callback (fun ev ->
          ev##target##style##display <- string "none")
        ) *)
    )
    )
