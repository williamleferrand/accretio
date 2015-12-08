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


{shared{

open Lwt

}}

{server{

open Vault

let report_error message =
  Sessions.get () >>= function
  | Anonymous ->
    Lwt_log.ign_error_f "Error caught: [session:Anonymous] %s" message ;
    return_unit
  | Connected session ->
    Lwt_log.ign_error_f
      "Error caught: [session:%s] %s" (Ys_uid.to_string session.member_uid)
      message ;
    return_unit

let report_error =
  server_function ~name:"report-error" Json.t<string> report_error

}}

{client{

open React

open Eliom_content.Html5
open Eliom_content.Html5.D

type message =
    Silent
  | Error of string
  | Warning of string
  | Ask of string * (bool -> unit)

let message, update_message = S.create Silent

(* helpers ********************************************************************)

let error message =
  Ys_mixpanel.track "ask-error" ~params:[ "message", message ] () ;
  Lwt.ignore_result (%report_error message);
  update_message (Error message)

let warning message =
  Ys_mixpanel.track "ask-warning" ~params:[ "message", message ] () ;
  update_message (Warning message)

let ask message continuation =
  Ys_mixpanel.track "help-ask" ~params:[ "message", message ] () ;
  update_message (Ask (message, continuation))

let silent () = update_message Silent

let silent_on_keydown input =
  Manip.Ev.onkeydown input (fun _ -> update_message Silent; true)

let connection_lost () =
  (* don't log an error here *)
  (* warning "Connection lost ## please refresh the page" ; *)
  (* Dom_html.window##location##reload() *)
  ()

}}
