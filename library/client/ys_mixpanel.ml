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


open Eliom_content.Html5.D
open Lwt
open Js

class type people = object
  method increment : js_string t -> int -> unit meth
  method identify: js_string t-> unit meth
  method set: 'a t -> unit meth
end

class type mixpanel = object
  method track:js_string t -> unit meth
  method track_2: js_string t-> js_string t-> unit meth
  method identify: js_string t-> unit meth
  method alias: js_string t-> unit meth
  method _register_once_ : 'a t -> unit meth
  method register: 'a t -> unit meth
  method people: people t readonly_prop
  method _name_tag_: js_string t -> unit meth
end

let if_active op =
  let mixpanel = Js.Unsafe.get Dom_html.window (Js.string "mixpanel") in
  if Js.Optdef.test mixpanel
  then
    if Js.to_bool
        (Js.Unsafe.get
           mixpanel
           (Js.string "__loaded")) then op mixpanel

let escape e =
  let json = Js.Unsafe.variable "JSON" in
  Js.Optdef.case
    json
    (fun () ->
       "\"" ^ (Js.to_string (Js.escape (Js.string e))) ^ "\"")
    (fun json ->
       Js.to_string (json##stringify (Js.string e)))

let js_string_from_list l =
  "({"
  ^ (String.concat ", " (List.map (fun (n,v) -> let e = escape v in
      Printf.sprintf "'%s' : %s" n e) l))
  ^"})"

let set_people params =
  if_active
    (fun mixpanel ->
       let js_string = js_string_from_list params in
       let js_object = Js.Unsafe.eval_string js_string in
       mixpanel##people##set (js_object))

let track label ?params () =
  if_active
    (fun mixpanel ->
       let label = Js.string label in
       match params with
       | Some [] | None -> mixpanel##track(label)
       | Some params -> let js_string = js_string_from_list params in
         let js_object = Js.Unsafe.eval_string js_string in
         mixpanel##track_2 (label, js_object))

let identify viewer_uid =
  if_active
    (fun mixpanel ->
       let string_uid = Ys_uid.to_string viewer_uid in
       let js_string_uid = Js.string string_uid in
       mixpanel##identify (js_string_uid))

let alias viewer_uid =
  if_active
    (fun mixpanel ->
       let string_uid = Ys_uid.to_string viewer_uid in
       let js_string_uid = Js.string string_uid in
       mixpanel##alias (js_string_uid))

let name_tag name =
  if_active
    (fun mixpanel ->
       mixpanel##_name_tag_ (Js.string name))

let register_once key value =
  if_active
    (fun mixpanel ->
       let param = "({'"^key^"': '"^value^"'})" in
       let js_object = Js.Unsafe.eval_string param in
       mixpanel##_register_once_ (js_object))

let register key value =
  if_active
    (fun mixpanel ->
       let param = "({'"^key^"': '"^value^"'})" in
       let js_object = Js.Unsafe.eval_string param in
       mixpanel##register (js_object))

let increment param ?(n=1) () =
  if_active
    (fun mixpanel ->
       mixpanel##people##increment(Js.string param, n))
