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

open React
open Eliom_content.Html5.D
open Vault

open Ys_uid

(* this should be replaced by client-side services as soon as they become
   available *)

(* uid is replaced by int to simplify the use of deriving *)

type manage_step =
    ManageHome
  | ManageMailboxes
  | ManageMembers
  | ManageCustom deriving (Json)

type service =
  | Uninitialized
  (* admin services *)
  | AdminGraphMember of int option
  | AdminGraphThread of int option
  | AdminStats
  | AdminI18n
  | Settings
  | RequestRecovery
  | Recover of string
  | Member of int
  | Feedback
  | Landing
  (* new services 9/21 *)
  | Playbook of int
  | Dashboard
  | Create
  | Society of string * int
  | Manage of string * int * manage_step
  | Payment of string * int
  | Search of string option
  | Schoolbus
  | Library deriving(Json)

let admin objekt =
  function
  | None -> [ "admin" ; "graph" ; objekt ]
  | Some uid -> [ "admin" ; "graph" ; objekt ; Ys_uid.to_string uid ]

}}

{client{

let path_of_service = function
    Uninitialized -> [ "" ]
  | AdminGraphMember uid_option -> admin "member" uid_option
  | AdminGraphThread uid_option -> admin "thread" uid_option
  | AdminStats -> [ "admin" ; "stats" ]
  | AdminI18n -> [ "admin" ; "i18n" ]
  | Settings -> [ "settings" ]
  | RequestRecovery -> [ "request_recovery" ]
  | Recover token -> [ "recover" ; token ]
  | Member uid -> [ "member" ; Ys_uid.to_string uid ]
  | Feedback -> [ "feedback" ]
  | Playbook uid -> [ "playbook" ; Ys_uid.to_string uid ]
  | Dashboard -> [ "dashboard" ]
  | Library -> [ "library" ]
  | Create -> [ "create" ]
  | Society (shortlink, _) -> [ "society" ; shortlink ]
  | Manage (shortlink, _, ManageHome) -> [ "manage" ; shortlink ]
  | Manage (shortlink, _, ManageMailboxes) -> [ "manage" ; "mailboxes" ; shortlink ]
  | Manage (shortlink, _, ManageMembers) -> [ "manage" ; "members" ; shortlink ]
  | Manage (shortlink, _, ManageCustom) -> [ "manage" ; "custom" ; shortlink ]
  | Payment (shortlink, _) -> [ "payment" ; shortlink ]
  | Search None -> [ "search" ]
  | Search (Some query) -> [ "search" ; query ]
  | Schoolbus -> [ "schoolbus" ]
  | Landing -> [ "" ]

let drop_search s1 s2 =
  match s1, s2 with
  | Search _, Search _ -> true
  | Landing, Search _ -> true
  | _, _ -> false

let service, update_service = S.create ~eq:drop_search Uninitialized

let update_service service =
  (* Popup.close () ; *)
  update_service service

(* we rely on the browser stack *)
let push_state_enable = Dom_html.hasPushState ()

let goto service =
  let state = Deriving_Json.to_string Json.t<service> service in
  let uri = String.concat "/" (path_of_service service) in
  Ys_mixpanel.track
   ~params:[ "uri", uri ;
             "state", state]
   "goto" () ;
  if push_state_enable then
    Dom_html.window##history##pushState(Js.string state, Js.string "", Js.some (Js.string ("/" ^ uri))) ;
  update_service service (* assuming that we have the proper comparison function,
                            we should be fine ... *)

let init () =
  if push_state_enable
  then
    Dom_html.window##onpopstate <- Dom_html.handler
        (fun e ->
           let state = Js.to_string (Obj.magic e##state) in
           (try
              let service = Deriving_Json.from_string Json.t<service> state in
              update_service service ;
            with _ -> ());
           Js._true)

}}
