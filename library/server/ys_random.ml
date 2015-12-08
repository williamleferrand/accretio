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
open Lwt_preemptive

let _ =
  Ys_config.if_prod Random.self_init

let random_string =
  Random.self_init ();
  (fun size ->
     let s = Bytes.create size in
     for i = 0 to size - 1 do
       Bytes.set s i (Char.chr (97 + Random.int 26))
     done ;
     return (Bytes.to_string s))

let rec random_string_unique ?(max_iterations = 32) check size =
  if (max_iterations < 0) then
    return_none
  else
    lwt s = random_string size in
    match_lwt check s with
    | true -> return (Some s)
    | false -> random_string_unique ~max_iterations:(max_iterations - 1) check size
