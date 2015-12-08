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
open Js
open Js.Unsafe

let countdown deadline =
  let dom, update_dom = S.create "" in
  let units =
    (((Js.Unsafe.variable "countdown")##_DAYS) lor (Js.Unsafe.variable "countdown")##_HOURS)
    lor (((Js.Unsafe.variable "countdown")##_MINUTES) lor (Js.Unsafe.variable "countdown")##_SECONDS)
  in

  fun_call
    (variable "countdown")
    [| inject (jsnew Js.date_fromTimeValue (Int64.to_float deadline *. 1000.)) ;
       inject (wrap_callback (fun ts -> update_dom (Js.to_string ts##toString()))) ;
       inject units |] ;
  dom
