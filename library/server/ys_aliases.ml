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

open Mysql

let regex = Str.regexp "\?"

let strip = Str.global_replace regex ""

let sphinx_pool =
  Lwt_pool.create
    ~validate:(Lwt_preemptive.detach
                 (fun dbd ->
                    (* this might be an expensive call *)
                    try
                      Mysql.ping dbd;
                      Printf.printf "Pinging sphinx, positive\n" ;
                      flush stdout ;
                      true
                    with exn ->
                      Printf.printf "Pinging sphinx, negative (%s)\n" (Printexc.to_string exn) ;
                      flush stdout ;
                      false))
    (1)
    (Lwt_preemptive.detach
       (fun () ->
          let dbd = Mysql.quick_connect ~host:"127.0.0.1" ~port:9306 () in
          (* Mysql.set_charset dbd "utf8" ; *)
          dbd))


exception MysqlError of error_code

module PlainText =
struct

  let insert table uid text timestamp =
    let text = strip text in
    Lwt_pool.use sphinx_pool
      (Lwt_preemptive.detach
         (fun dbd ->
            let req =
              Printf.sprintf "REPLACE INTO %s (id, description, created_ts) VALUES (%s, '%s', %s)"
                table
                (Ys_uid.to_string uid)
                (real_escape dbd text)
                (Ys_time.to_string timestamp)
            in
            ignore (exec dbd req) ;
            match status dbd with
            | StatusOK -> ()
            | StatusEmpty -> ()
            | StatusError error -> raise (MysqlError error)))

  let search table query =
    let query = strip query in
    Lwt_pool.use sphinx_pool
      (fun dbd ->
         let req =
           Printf.sprintf "SELECT id FROM %s WHERE MATCH('%s') ORDER BY created_ts DESC"
             table
             (real_escape dbd query) in
         lwt result = Lwt_preemptive.detach (exec dbd) req in
         let stream = Lwt_stream.from_direct
             (fun () ->
                match fetch result with
                | None -> None
                | Some row ->
                  match row.(0) with
                    None -> None
                  | Some id -> Some (Ys_uid.of_string id)) in
         Lwt_stream.to_list stream)

end


module PlainTextEdge =
struct

  let id uid edge =  Hashtbl.hash (Printf.sprintf "%d-%d" uid edge)

  let remove table uid edge =
    Lwt_pool.use sphinx_pool
      (Lwt_preemptive.detach
         (fun dbd ->
            let id = id uid edge in
            let req =
              Printf.sprintf "DELETE FROM %s WHERE id=%d"
                table
                id
            in
            ignore (exec dbd req) ;
            match status dbd with
            | StatusOK -> ()
            | StatusEmpty -> ()
            | StatusError error -> raise (MysqlError error)))

  let insert table uid edge text =
    Lwt_log.ign_info_f "inserting uid: %d, edge, %d, text: %s into %s" uid edge text table ;
    let text = strip text in
    Lwt_pool.use sphinx_pool
      (Lwt_preemptive.detach
         (fun dbd ->
            let id = id uid edge in
            let req =
              Printf.sprintf "REPLACE INTO %s (id, host, edge, description) VALUES (%d, %d, %d, '%s')"
                table
                id
                uid
                edge
                (real_escape dbd text)
            in
            ignore (exec dbd req) ;
            match status dbd with
            | StatusOK -> ()
            | StatusEmpty -> ()
            | StatusError error -> raise (MysqlError error)))

  let search table query host =
    let query = strip query in
    Lwt_pool.use sphinx_pool
      (fun dbd ->
         let req =
           Printf.sprintf "SELECT edge FROM %s WHERE MATCH('%s') AND host = %d"
             table
             (real_escape dbd query)
             host
         in
         lwt result = Lwt_preemptive.detach (exec dbd) req in
         let stream = Lwt_stream.from_direct
             (fun () ->
                match fetch result with
                | None -> None
                | Some row ->
                  match row.(0) with
                    None -> None
                  | Some id -> Some (Ys_uid.of_string id)) in
         Lwt_stream.to_list stream)

end


module Spatial =
struct

  type index =
    {
      mutable rtree : Ys_uid.uid Rtree.t
    }

  let create () =
    {
      rtree = Rtree.empty
    }

  let insert index uid envelope =
    index.rtree <- Rtree.insert index.rtree uid (Ys_envelope.to_tuple envelope)

  let search index envelope =
    Rtree.find index.rtree (Ys_envelope.to_tuple envelope)

end
