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



(* ocsipersist storage *)

module OcsipersistStorage =
struct

  let open_db_blocking ~cache_size name =
    let name = Filename.basename name in
    Ocsipersist.open_table name

  let get db key =
    try_lwt
      lwt value = Ocsipersist.find db key in
      return (Some value)
    with Not_found -> return_none

  let put db key value =
    Ocsipersist.add db key value

  let delete db key =
    Ocsipersist.remove db key

  module Batch =
  struct

    let get db keys =
      lwt results =
        Lwt_list.map_s
          (fun key -> get db key)
          (Array.to_list keys)
      in
      return (Array.of_list results)

  end

  let max_key db =
    try_lwt
      lwt max_id =
        Ocsipersist.fold_step
          (fun key _ acc ->
             return (max (Ys_uid.extract_from_key key) acc))
          db
          0
      in
      return (Some max_id)
    with _ -> return_none

end


module LevelDBStorage =
struct

  let open_db_blocking ~cache_size = LevelDB.open_db ~cache_size

  let get db = detach (LevelDB.get db)

  let put db key value =
    LevelDB.put ~sync:false db key value ;
    return_unit

  let delete db key =
    LevelDB.delete ~sync:false db key ;
    return_unit

  module Batch =
  struct

    let get db = detach (Array.map (LevelDB.get db))

  end

  let max_key db =
    try_lwt
      let iterator = LevelDB.Iterator.make db in
      LevelDB.Iterator.seek_to_last iterator ;
      match LevelDB.Iterator.valid iterator with
      | false -> return_none
      | true -> return (Some (Ys_uid.extract_from_key (LevelDB.Iterator.get_key iterator)))
    with _ -> return_none

end



module Siphon =
struct

  let from_leveldb_to_ocsipersist name =
    Lwt_log.ign_info_f "migrating data from leveldb to ocsipersist for table %s" name ;
    let dbin = LevelDBStorage.open_db_blocking ~cache_size:0 name in
    let dbout = OcsipersistStorage.open_db_blocking ~cache_size:0 name in

    let it = LevelDB.Iterator.make dbin in
    LevelDB.Iterator.seek_to_first it ;

    let rec drain () =
      match LevelDB.Iterator.valid it with
        false -> return_unit
      | true ->
        let key = LevelDB.Iterator.get_key it in
        let value = LevelDB.Iterator.get_value it in
        lwt _ = OcsipersistStorage.put dbout key value in
        LevelDB.Iterator.next it ;
        drain ()
    in

    lwt _ = drain () in

    LevelDB.close dbin ;
    Lwt_log.ign_info_f "transfer done!" ;
    return_unit


  let from_ocsipersist_to_leveldb dbout name =
    Lwt_log.ign_info_f "migrating data from ocsipersist to leveldb for table %s" name ;
    let dbin = OcsipersistStorage.open_db_blocking ~cache_size:0 name in
    lwt _ =
      Ocsipersist.iter_step
        (LevelDBStorage.put dbout)
        dbin
    in
    Lwt_log.ign_info_f "done transferring table %s" name ;
    return_unit ;

end


module Store = LevelDBStorage
