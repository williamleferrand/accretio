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
open Sessions
open Ys_uid
open Vault

}}

{server{

let get_playbooks_ () =
  solve_acl
    ACLConnected
    (fun session ->
       Lwt_log.ign_info_f "getting playbooks for member %d" session.member_uid ;
       lwt views, _ =
         Object_playbook.Store.fold_flat_lwt
           (fun views uid ->
              match Registry.mem uid with
                false -> return (Some views)
              | true ->
                Lwt_log.ign_info_f "inspecting playbook %d" uid ;
                match_lwt $playbook(uid)->(scope, owner) with
                  Ys_scope.Public, _ -> lwt view = View_playbook.to_view uid in return (Some (view :: views))
                | _, owner when owner = session.member_uid -> lwt view = View_playbook.to_view uid in return (Some (view :: views))
                | _, owner ->
                  Lwt_log.ign_info_f "skipping playbook owned by %d" owner ;
                  return (Some views))
             []
             None
             (-1)
         in
         return views
    )
    (fun () ->
       Lwt_log.ign_info_f "getting all public playbooks" ;
       lwt views, _ =
         Object_playbook.Store.fold_flat_lwt
           (fun views uid ->
              match Registry.mem uid with
                false -> return (Some views)
              | true ->
                match_lwt $playbook(uid)->scope with
                  Ys_scope.Public -> lwt view = View_playbook.to_view uid in return (Some (view :: views))
                | _ -> return (Some views))
          []
          None
          (-1)
      in
      return views)

let get_playbooks = server_function ~name:"dashboard-get-playbooks" Json.t<unit> get_playbooks_

}}

{client{

open React
open Ys_react
open Eliom_content.Html5
open Eliom_content.Html5.D
open View_society

let builder playbooks =
  let playbooks = RList.init playbooks in
  let playbooks = RList.map_in_div ~a:[ a_class [ "playbooks" ]] View_playbook.format playbooks in
  div ~a:[ a_class [ "library" ]] [
    h1 [ pcdata "Library" ] ;
    playbooks
  ]

let dom =
  Template.apply
    %get_playbooks
    builder

 }}
