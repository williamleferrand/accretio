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


open Printf
open Mysql

let insert source timestamp message =
  Lwt_pool.use
    Ys_aliases.sphinx_pool
    (Lwt_preemptive.detach
       (fun dbd ->
          (* timestamp is in second, which means that we can't log the same
             message more than once per second *)
          let id = Hashtbl.hash (sprintf "%Ld-%s" (Int64.add (Int64.mul timestamp 100L) (Int64.of_int (Random.int 100))) message) in
          let req =
            Printf.sprintf "INSERT INTO logs (id, source, timestamp, message, message_text) VALUES (%d, '%s', '%s', '%s', '%s')"
              id
              (real_escape dbd source)
              (Ys_time.to_string timestamp)
              (real_escape dbd message)
              (real_escape dbd message)
          in
          ignore (exec dbd req) ;
          match status dbd with
          | StatusOK -> ()
          | StatusEmpty -> ()
          | StatusError error -> raise (Ys_aliases.MysqlError error)))

let search source query start_ end_ =
  lwt result =
    Lwt_pool.use
    Ys_aliases.sphinx_pool
    (Lwt_preemptive.detach
       (fun dbd ->
          let req =
            Printf.sprintf "SELECT timestamp, message_text FROM logs WHERE MATCH('%s') AND source = '%s' ORDER BY timestamp DESC"
              (real_escape dbd query)
              (real_escape dbd source)
          in
          let result = exec dbd req in
          match status dbd with
          | StatusOK -> result
          | StatusEmpty -> failwith "no result"
          | StatusError error -> raise (Ys_aliases.MysqlError error))) in
  let stream =
    Lwt_stream.from_direct
      (fun () ->
         match fetch result with
         | None -> None
         | Some row ->
           match row.(0), row.(1) with
             Some timestamp, Some message -> Some (int_of_string timestamp, message)
           | _ -> None)
  in
  Lwt_stream.to_list stream


let list_all_from_society society since =
  lwt result =
    Lwt_pool.use
    Ys_aliases.sphinx_pool
    (Lwt_preemptive.detach
       (fun dbd ->
          let req =
            Printf.sprintf "SELECT timestamp, message_text, source, (source = 'context-info-%d' OR source = 'context-warning-%d' OR source = 'context-error-%d') AND timestamp > %Ld AS mycond FROM logs WHERE mycond = 1 ORDER BY timestamp ASC"
              society
              society
              society
              since

          in
          let result = exec dbd req in
          match status dbd with
          | StatusOK -> result
          | StatusEmpty -> failwith "no result"
          | StatusError error -> raise (Ys_aliases.MysqlError error))) in
  let stream =
    Lwt_stream.from_direct
      (fun () ->
         match fetch result with
         | None -> None
         | Some row ->
           match row.(0), row.(1), row.(2) with
             Some timestamp, Some message, Some source -> Some (Int64.of_string timestamp, (message, Int64.of_string timestamp, source))
           | _ -> None)
  in
  Lwt_stream.to_list stream
