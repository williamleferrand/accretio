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
open Eliom_content.Html5
open Eliom_content.Html5.D


let wrap printer fmt =
  Printf.ksprintf (fun s -> printer (Js.string s)) fmt

let debug fmt =
  Printf.ksprintf (fun s -> Firebug.console##debug (Js.string s)) fmt
let error fmt =
  Printf.ksprintf (fun s -> Firebug.console##error (Js.string s)) fmt
let warning fmt =
  Printf.ksprintf (fun s -> Firebug.console##warn (Js.string s)) fmt
let info fmt =
  Printf.ksprintf (fun s -> Firebug.console##info (Js.string s)) fmt
