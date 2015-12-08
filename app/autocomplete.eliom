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

let users prefix =
  try_lwt
    let query = Printf.sprintf "%s*" prefix in
    lwt uids = Object_member.Store.search_name query in
    Lwt_log.ign_info_f "found %d results for prefix %s (query: %s)" (List.length uids) prefix query ;
    List.iter (fun uid ->
        Lwt_log.ign_info_f "uid: %d" uid) uids ;
    lwt views = Lwt_list.map_p View_member.to_view uids in
    return (Some views)
  with _ -> return (Some [])

let users = server_function ~name:"autocomplete-users" Json.t<string> users

}}

{client{

open Eliom_content.Html5
open Eliom_content.Html5.D

let create_user ?(placeholder="") format wrapper select =
  Ys_typeahead.create ~placeholder (detach_rpc %users) View_member.uid format wrapper select

}}
