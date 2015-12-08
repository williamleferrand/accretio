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
open Ys_uid

type session_connected = {
  member_uid : uid ;
  member_name : string ;
  member_show_introduction : bool ;
  member_is_admin : bool ;
  member_member : View_member.t ;
}

type session =
    Anonymous
  | Connected of session_connected

}}

{server{

let sessions : uid option Eliom_reference.eref =
  Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope
    ~persistent:"__mu_session_cookie"
    None

let get () = Eliom_reference.get sessions
let set uid = Eliom_reference.set sessions (Some uid)

let unset () = Eliom_reference.unset sessions

let create_session uid =
  lwt member = View_member.to_view uid in
  match_lwt $member(uid)->(name, settings, rights) with
  | (member_name, settings, Object_member.Admin) ->
    return { member_uid = uid ;
             member_name ;
             member_show_introduction = settings.Object_member.show_introduction ;
             member_is_admin = true ; member_member = member }
  | (member_name, settings, _) ->
    return { member_uid = uid ;
             member_name ;
             member_show_introduction = settings.Object_member.show_introduction ;
             member_is_admin = false ; member_member = member }

(* acl management *************************************************************)

type acl = ACLConnected | ACLAdmin

let solve_acl acl success failure =
  match_lwt get () with
  | None ->
    failure ()
  | Some uid ->
    lwt session = create_session uid in
    match acl with
    | ACLConnected -> success session
    | ACLAdmin ->
      if session.member_is_admin then
        success session
      else
        failure ()

let protected_connected callback =
  try_lwt
    solve_acl ACLConnected callback (fun () -> return_none)
  with exn ->
    Lwt_log.ign_error_f "exception caught in protected_connected: %s" (Printexc.to_string exn) ;
    return_none

let protected_admin callback =
  solve_acl ACLAdmin callback (fun () -> return_none)

let protected_admin_bool callback =
  solve_acl ACLAdmin
    (fun session -> callback session)
    (fun () -> return_false)

(* let store the AWS credentials here as well ********************************)

let creds =
  {
    Creds.aws_access_key_id =
      Ys_config.get_string "aws-access-key-id" ;
    Creds.aws_secret_access_key =
      Ys_config.get_string "aws-secret-access-key" ;
  }

(* some convenient tools ******************************************************)

let session_to_string = function
  | Anonymous -> "anonymous"
  | Connected session ->
    Printf.sprintf
      "connected (uid=%d, name=%s, is_admin=%b)"
      session.member_uid
      session.member_name
      session.member_is_admin

(* new acl protection *********************************************************)

let log_session session =
  Printf.sprintf "{ 'uid': %d, 'name': '%s' }" session.member_uid session.member_name

}}

{client{

let rpc meth arg callback =
  try_lwt
    match_lwt meth arg with
    | None -> Help.warning "Something went wrong" ; return_unit
    | Some result -> callback result ; return_unit
  with exn -> Help.warning "Something went wrong" ; return_unit

let detach_rpc meth arg callback =
  ignore_result (rpc meth arg callback)

}}
