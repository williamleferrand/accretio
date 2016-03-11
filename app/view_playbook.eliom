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

type scope = Public | Private

type parameter =
  {
    label : string ;
    key : string ;
  }

type property =
  {
    property_name : string ;
    property_value : string ;
  }

type t =
  {
    uid : uid ;
    owner : View_member.t ;
    name : string ;
    description : string ;
    hash : string ;
    scope : scope ;
    parameters : parameter list ;
    properties : property list ;
  }

}}

{server{


let to_view uid =
  lwt owner, name, description, hash, scope, parameters, properties = $playbook(uid)->(owner, name, description, hash, scope, parameters, properties) in
  lwt owner = View_member.to_view owner in
  let scope =
    match scope with
      Ys_scope.Public -> Public
    | Ys_scope.Private -> Private
  in
  let parameters =
    List.map
      (fun parameter -> { label = parameter.Object_playbook.label ; key = parameter.Object_playbook.key })
      parameters
  in
  let properties =
    List.map
      (fun property -> { property_name = property.Object_playbook.property_name ;
                         property_value = property.Object_playbook.property_value })
      properties
  in
  return {
    uid ;
    owner ;
    name ;
    description ;
    hash ;
    scope ;
    parameters ;
    properties ;
  }

}}

{client{

open React
open Ys_react
open Eliom_content.Html5
open Eliom_content.Html5.D

let format view =
  div ~a:[ a_class [ "playbook" ; "view-playbook" ] ;
           a_onclick (fun _ -> Service.goto (Service.Playbook view.uid)) ] [
    div ~a:[ a_class [ "name" ]] [ pcdata view.name ] ;
    div ~a:[ a_class [ "description" ]] [ pcdata view.description ] ;
    (match view.properties with
       [] -> pcdata ""
     | _ as properties ->
       div ~a:[ a_class [ "properties" ]] [
         ul (List.map (fun property -> li [ h3 [ pcdata property.property_name ] ;
                                            pcdata property.property_value ]) properties)
    ])
  ]

}}
