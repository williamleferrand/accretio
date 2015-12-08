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


open Lwt

module StringSet = Set.Make(String)

let reserved = ref StringSet.empty

let checkers : (string -> int option Lwt.t) list ref = ref []

let check_if_shortlink_is_available shortlink =
  match_lwt Lwt_list.exists_p
              (fun checker ->
                 match_lwt checker shortlink with
                   None -> return_false
                 | Some _ -> return_true) !checkers with
  | true -> return_false
  | false -> return (not (StringSet.mem shortlink !reserved))

let reserve_lock = Lwt_mutex.create ()

let reserve () =
  Lwt_mutex.with_lock
    reserve_lock
    (fun () ->
       match_lwt Ys_random.random_string_unique check_if_shortlink_is_available 5 with
         None -> return_none
       | Some shortlink ->
         Lwt_log.ign_info_f "reserving shortlink %s" shortlink ;
         reserved := StringSet.add shortlink !reserved ;
         return (Some shortlink))

let release shortlink =
  reserved := StringSet.remove shortlink !reserved

let register_checker checker =
  checkers := checker :: !checkers


let create () =
  let rec aux = function
      0 -> return_none
    | n -> match_lwt reserve () with
        None -> aux (n-1)
      | Some code -> return (Some code)
  in
  aux 5
