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


{client{

open Lwt
open React
open Eliom_content.Html5.D

(*

(* this template will wait for data to come from the fetcher before applying
   the builder *)

let apply fetcher builder uid : Html5_types.div_content_fun elt option React.signal =
  let details, update_details = S.create None in
  ignore_result (try_lwt lwt details = fetcher uid in update_details (Some details) ; return_unit) ;
  S.map
    (function
      | None -> None
      | Some details -> Some (builder details))
    details

*)

(* this template will call the fetcher on startup and at each session change *)


let apply_and_refresh fetcher builder uid : Html5_types.div_content_fun elt option React.signal =
  let details, update_details = S.create None in
  ignore
    (S.map
       (fun _ ->
          ignore_result
            (try_lwt lwt details = fetcher uid in update_details (Some details) ; return_unit))
       Sessions.session) ;
  S.map
    (function
      | None -> None
      | Some details -> Some (builder details))
    details


let apply fetcher builder uid : Html5_types.div_content_fun elt option React.signal =
  let details, update_details = S.create None in
  ignore_result
    (try_lwt lwt details = fetcher uid in update_details (Some details) ; return_unit) ;
  S.map
    (function
      | None -> None
      | Some details -> Some (builder details))
    details

}}
