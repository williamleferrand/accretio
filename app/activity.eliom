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

let retrieve_activity uid =
  protected_connected
    (fun session ->
       lwt view = View_activity.to_view uid in
       return (Some view))

let retrieve_activity = server_function ~name:"activity-retrieve-activity" Json.t<int> retrieve_activity

}}


{client{

open React
open Ys_react
open Eliom_content.Html5
open Eliom_content.Html5.D

let builder = function
  | None -> pcdata ""
  | Some activity ->
    div ~a:[ a_class [ "activity" ]] [


    ]

let dom = Template.apply %retrieve_activity builder


}}
