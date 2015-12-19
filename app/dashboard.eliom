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

let get_societies () =
  protected_connected
    (fun session ->
       Lwt_log.ign_info_f "getting societies for member %d" session.member_uid ;
       lwt societies = $member(session.member_uid)->societies in
       let societies = Edges.uids societies in
       lwt societies = Lwt_list.map_p View_society.to_view societies in
       Lwt_log.ign_info_f "getting societies for member %d, found %d elements" session.member_uid (List.length societies) ;
       return (Some societies))

let get_societies = server_function ~name:"dashboard-get-societies" Json.t<unit> get_societies

}}

{client{

open React
open Ys_react
open Eliom_content.Html5
open Eliom_content.Html5.D
open View_society

let builder = function
  | None ->
    Service.goto Service.Landing ;
    div []
  | Some societies ->
    let societies = RList.init societies in
    let societies = RList.map_in_div View_society.format societies in
    div ~a:[ a_class [ "dashboard" ]] [
      societies ;
    ]

let dom =
  Template.apply
    %get_societies
    builder

 }}
