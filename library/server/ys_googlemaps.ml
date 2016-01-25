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
open Eliom_content.Html5.D

(* server-side gmaps **********************************************************)

exception InvalidResult of string

let extract_location location =
  match location with
    `Assoc l ->
    (match List.assoc "lat" l, List.assoc "lng" l with
       `Float lat, `Float lon -> (lat, lon)
     | _ -> raise (InvalidResult "invalid json"))
  | _ -> raise (InvalidResult "invalid json")

let extract_result result =
    match result with
      | `Assoc l ->
        (match List.assoc "geometry" l with
         | `Assoc l ->
           extract_location (List.assoc "location" l)
         | _ -> raise (InvalidResult "invalid json"))
      | _ -> raise (InvalidResult "invalid json")

let rec fetch address =
  let parameters = [ "sensor", "false" ; "address", address ] in
  let url = Printf.sprintf "http://maps.googleapis.com/maps/api/geocode/json?%s" (Netencoding.Url.mk_url_encoded_parameters parameters) in
  try_lwt
    lwt s, _ = Ys_http.get_url url () in
    match Yojson.Basic.from_string s with
      `Assoc l ->
      (match List.assoc "status" l with
       | `String "OK" ->
         (match List.assoc "results" l with
          | `List results -> return (List.map extract_result results)
          | _ -> fail (InvalidResult "invalid json"))
       | `String status -> return []
       | _ -> fail (InvalidResult "invalid json"))
    | _ -> fail (InvalidResult "invalid json")
  with _ -> fail (InvalidResult "call faild")

let fetch_raw address =
  let parameters = [ "sensor", "false" ; "address", address ] in
  let url = Printf.sprintf "http://maps.googleapis.com/maps/api/geocode/json?%s" (Netencoding.Url.mk_url_encoded_parameters parameters) in
  try_lwt
    lwt s, _ = Ys_http.get_url url () in
    match Yojson.Basic.from_string s with
      `Assoc l ->
      (match List.assoc "status" l with
       | `String "OK" ->
         (match List.assoc "results" l with
          | `List results -> return results
          | _ -> fail (InvalidResult "invalid json"))
       | `String status -> return []
       | _ -> fail (InvalidResult "invalid json"))
    | _ -> fail (InvalidResult "invalid json")
  with _ -> fail (InvalidResult "call faild")


(* the js to use on the client ************************************************)

let script () =
  script ~a:[ a_src (uri_of_string (fun () -> "/js-map.js")) ] (pcdata "")
