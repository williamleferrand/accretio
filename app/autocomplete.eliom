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
open Vault
open Sessions
open React

}}

{server{

let autocomplete label searcher viewer =
  let handler prefix =
    try_lwt
      let query = Printf.sprintf "%s*" prefix in
      lwt uids = searcher query in
      Lwt_log.ign_info_f "found %d results for prefix %s (query: %s)" (List.length uids) prefix query ;
      List.iter (fun uid ->
          Lwt_log.ign_info_f "uid: %d" uid) uids ;
      lwt views = Lwt_list.map_p viewer uids in
      return (Some views)
    with _ -> return (Some [])
  in
  server_function ~name:("autocomplete-"^label) Json.t<string> handler

let users = autocomplete "users" Object_member.Store.search_name View_member.to_view
let societies = autocomplete "societies" Object_society.Store.search_name View_society.to_view
let playbooks = autocomplete "playbooks" Object_playbook.Store.search_name View_playbook.to_view


}}

{client{

open Eliom_content.Html5
open Eliom_content.Html5.D

let create_user ?(placeholder="") format wrapper select =
  Ys_typeahead.create ~placeholder (detach_rpc %users) View_member.uid format View_member.name wrapper select

let create_society ?(placeholder="") format wrapper select =
  Ys_typeahead.create ~placeholder (detach_rpc %societies) View_society.uid format View_society.name wrapper select

let create_playbook ?(placeholder="") format wrapper select =
  Ys_typeahead.create ~placeholder (detach_rpc %playbooks) View_playbook.uid format View_playbook.name wrapper select

}}
