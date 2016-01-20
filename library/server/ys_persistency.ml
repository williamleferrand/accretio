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


(* right now, this module is a thin lwt layer on top of LevelDB *)
(* I'm thinking about removing this module *)

open Lwt
open Lwt_preemptive

let open_db_blocking ~cache_size = LevelDB.open_db ~cache_size

let shutdown = detach LevelDB.close

let get_exn db = detach (LevelDB.get_exn db)
let get db = detach (LevelDB.get db)

(* we only use the async leveldb put *)
let put =
  LevelDB.put ~sync:true

let delete =
  LevelDB.delete ~sync:true

module Batch =
struct

  let get_exn db = detach (Array.map (LevelDB.get_exn db))
  let get db = detach (Array.map (LevelDB.get db))

end

let max_key_blocking db =
  let iterator = LevelDB.Iterator.make db in
  LevelDB.Iterator.seek_to_last iterator ;
  match LevelDB.Iterator.valid iterator with
     | false -> None
     | true -> Some (LevelDB.Iterator.get_key iterator)
