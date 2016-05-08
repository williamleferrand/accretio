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

(* This is the second time we have custom blocks for a society, we would
   need to blend that somewhere in the API *)

{shared{

open Lwt
open Sessions
open Ys_uid
open Vault

open Children_schoolbus_types

}}

{server{

let fetch_activities society =
  protected_connected
    (fun _ ->
       lwt objects = $society(society)->objects in
       let activities =
         List.fold_left
           (fun acc ->
              function
              | (`Activity, uid) -> uid :: acc
              | _ -> acc)
           []
           objects
       in
       lwt activities = Lwt_list.map_s View_activity.to_view activities in
       return (Some activities))

let fetch_activities = server_function ~name:"schoolbus-planner-blocks-fetch-activities" Json.t<int> fetch_activities

}}

{client{

open React
open Ys_react
open Eliom_content.Html5
open Eliom_content.Html5.D

(* the activities manager *****************************************************)

let activities society =

  let activities = RList.create () in
  detach_rpc %fetch_activities society (RList.update activities) ;

  div ~a:[ a_class [ "schoolbus-planner-activities" ]] [
    h2 [ pcdata "Activities" ] ;
    RList.map_in_div View_activity.format activities
  ]

(* the doms *****************************************************************************)

let doms society data () =
  [
    activities society ;
  ]

}}
