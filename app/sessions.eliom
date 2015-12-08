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


{server{

open Lwt
open Lwt_react
open Eliom_react
open Vault

(* keep the invite code in a cookie *)

let invite_code : string option Eliom_reference.eref =
  Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope
    ~persistent:"__mu_invite_code_cookie"
    None

let set_invite_code code =
  Eliom_reference.set invite_code (Some code)

let get_invite_code () =
  Eliom_reference.get invite_code

let clear_invite_code () =
  Eliom_reference.unset invite_code

(* connect / disconnect users *************************************************)

let get () =
  match_lwt Vault.get () with
  | None -> return Anonymous
  | Some uid ->
    lwt session = Vault.create_session uid in
    (* Live.track (Live.Member uid) ; *)
    return (Connected session)

let connect uid =
  (* first we create the session *)
  lwt session = Vault.create_session uid in
  lwt _ = Vault.set uid in
  (* Live.track (Live.Member uid) ; *)
  return session

let disconnect () =
  match_lwt Vault.get () with
  | None -> return_unit
  | Some uid ->
    (* (try
       let target = Live.Member uid in
       Live.notify target Live.Disconnect ;
       Live.untrack target ;
       (* we should probably kill the pipe at this point *)
     with _ -> ()) ; *)
    Vault.unset ()

}}


{client{

open React
open Lwt_stream
open Ys_react
open Vault
open Live

let session, update_session = S.create Anonymous

let update_session = function
  | Anonymous as auth -> update_session auth
  | Connected session as auth ->
    Ys_mixpanel.identify session.member_uid ;
    update_session auth

let disconnect () =
  update_session Anonymous

let connect session =
  update_session (Connected session)

let stop_showing_introduction () =
  match S.value session with
    Anonymous -> ()
  | Connected session -> update_session (Connected { session with member_show_introduction = false })

let pipe : Live.op Lwt_stream.t option ref = ref None
let fb_params = ref Ys_facebook.default

let register_pipe pipe' =
  (* ignore
    (Lwt_stream.iter
       (function
         | Disconnect -> update_session Anonymous
         | Connect session -> update_session (Connected session)
         | HelpMessage message -> Help.warning message
         | _ -> ())
       pipe') ; *)
  pipe := Some pipe'

let register_fb_app_id fb_app_id =
  fb_params := { Ys_facebook.default with Ys_facebook.appId = fb_app_id }

(* let _ =
  RInt.iter
    (function
      | 0 -> Dom_html.document##title <- (Js.string "Accretio")
      | n -> Dom_html.document##title <- (Js.string (Printf.sprintf "(%d) Accretio" n)))
    Notifications.unread_notifications *)

}}
