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


open Eliom_content
open Html5
open D
open Lwt
open Js

let link dom =
  let d = Html5.To_dom.of_span dom in
  d##innerHTML <- ((Js.Unsafe.variable "Autolinker")##link (d##innerHTML, Js.Unsafe.variable "defaultAutolinkOptions"))

let link_rich dom =
  let d = Html5.To_dom.of_span dom in
  d##innerHTML <- ((Js.Unsafe.variable "Autolinker")##link (d##innerHTML, Js.Unsafe.variable "autolinkOptions"))

let attach_rich anchor wrapper =
  let anchor = Html5.To_dom.of_span anchor in
  let wrapper = Html5.To_dom.of_span wrapper in
  anchor##innerHTML <- ((Js.Unsafe.variable "Autolinker")##link (wrapper##innerHTML, Js.Unsafe.variable "autolinkOptions"))
