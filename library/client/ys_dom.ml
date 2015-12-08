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
open Eliom_content
open Html5
open D

let get_value dom = Js.to_string ((Html5.To_dom.of_input dom)##value)
let set_value e s = (Html5.To_dom.of_input e)##value <- Js.string s

let get_value_int dom =
  try
    Some (int_of_float (float_of_string (get_value dom)))
  with _ -> None

let get_value_float dom =
  try
    Some (float_of_string (get_value dom))
  with _ -> None

let get_value_textarea e = Js.to_string ((Html5.To_dom.of_textarea e)##value)
let set_value_textarea e s = (Html5.To_dom.of_textarea e)##value <- Js.string s

let get_value_select e = Js.to_string ((Html5.To_dom.of_select e)##value)
let set_value_select e s = (Html5.To_dom.of_select e)##value <- Js.string s

let check e = (Html5.To_dom.of_input e)##checked <- Js._true
let uncheck e = (Html5.To_dom.of_input e)##checked <- Js._false
let is_checked e = Js.to_bool (Html5.To_dom.of_input e)##checked

(* dom input that can be highlighted and reset when the user types sth in *****)

open React

module Highlighted =
  struct

    let trigger_onchange onchange =
      match onchange with
        None -> ()
      | Some callback -> callback ()

    let input ?onchange ~input_type ?(cls=[]) () =

      let highlighted, update_highlighted = S.create false in

      let dom =
        input
          ~a:[ R.a_class
                 (S.map
                    (function
                        false -> cls
                      | true -> "highlight" :: cls)
                    highlighted) ]
          ~input_type () in

      Manip.Ev.onkeydown
        dom
        (fun _ ->
           update_highlighted false;
           trigger_onchange onchange;
           true) ;

      dom, (fun () -> update_highlighted true)

    let textarea ?onchange ?(cls=[]) () =

      let highlighted, update_highlighted = S.create false in

      let dom =
        Raw.textarea
          ~a:[ R.a_class
                 (S.map
                    (function
                        false -> cls
                      | true -> "highlight" :: cls)
                    highlighted) ]
          (pcdata "")
      in

      Manip.Ev.onkeydown
        dom
        (fun _ ->
           update_highlighted false;
           trigger_onchange onchange;
           true) ;

      dom, (fun () -> update_highlighted true)


  end

let focus e = (Html5.To_dom.of_input e)##focus ()
let focus_textarea e = (Html5.To_dom.of_textarea e)##focus ()
let blur e = (Html5.To_dom.of_input e)##focus ()

let register_on_return dom callback =
  Manip.Ev.onkeydown
    dom
    (fun ev ->
       (match ev##keyCode with
        | code when code = Keycode.return -> callback ()
        | _ -> ());
       true)

let delay_focus dom =
  ignore_result (Lwt_js.yield () >>= fun _ -> focus dom ; return_unit)

let delay_focus_textarea dom =
  ignore_result (Lwt_js.yield () >>= fun _ -> focus_textarea dom ; return_unit)

let bridge anchor wrapper =
   let anchor = Html5.To_dom.of_span anchor in
  let wrapper = Html5.To_dom.of_span wrapper in
  anchor##innerHTML <- wrapper##innerHTML
