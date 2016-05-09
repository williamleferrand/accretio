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
open Vault
open View_society

}}

{server{

let retrieve uid =
  Lwt_log.ign_info_f "retrieving society %d" uid ;
  lwt leader = $society(uid)->leader in
  lwt is_leader =
    solve_acl
      ACLConnected
      (function
        | session when session.member_uid = leader -> return_true
        | session when session.member_is_admin -> return_true
        | _ -> return_false)
      (fun _ -> return_false)
  in
  match is_leader with
    true ->
    lwt view = Society_leader.retrieve uid in
    return (`SocietyLeader view)
  | false ->
    lwt view = View_society.to_view uid in
    (* let's get the public societies as well *)
    lwt societies = $society(uid)->societies in
    let societies = List.map snd societies in
    lwt societies = Lwt_list.filter_s (fun uid -> match_lwt $society(uid)->mode with Object_society.Public -> return_true | _ -> return_false) societies in
    lwt societies = Lwt_list.map_s View_society.to_view societies in
    return (`SocietyPublic (view, societies))

let retrieve = server_function ~name:"society-retrieve" Json.t<int> retrieve

}}

{client{

let builder = function
  | `SocietyLeader view -> Society_leader.dom view
  | `SocietyPublic (view, societies) -> Society_public.dom view societies

let dom = Template.apply %retrieve builder

}}
