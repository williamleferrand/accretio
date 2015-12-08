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

type t =
  {
    uid : uid ;
    owner : View_member.t ;
    name : string ;
    description : string ;
    hash : string ;
  }

}}

{server{

let to_view uid =
  lwt owner, name, description, hash = $playbook(uid)->(owner, name, description, hash) in
  lwt owner = View_member.to_view owner in
  return {
    uid ;
    owner ;
    name ;
    description ;
    hash ;
  }

}}

{client{

open React
open Ys_react
open Eliom_content.Html5
open Eliom_content.Html5.D

let format view =
  div ~a:[ a_class [ "playbook" ] ;
           a_onclick (fun _ -> Service.goto (Service.Playbook view.uid)) ] [
    div ~a:[ a_class [ "name" ]] [ pcdata view.name ] ;
    div ~a:[ a_class [ "description" ]] [ pcdata view.description ] ;
  ]

}}
