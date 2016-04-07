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
open Ocsigen_http_frame
open Ocsigen_stream

let generate_headers headers =
  List.fold_left (
    fun headers (label, value) ->
      let name = Http_headers.name label in
      Http_headers.add name value headers
  ) (Http_headers.empty) headers

let fragment_url (url : string) =
  let (https, host, port, uri, _, _, _) = Ocsigen_lib.Url.parse url in
  let host = match host with
    | None -> Lwt_log.ign_info_f "cannot extract host of url : %s" url; raise Not_found
    | Some h -> h in
  (https, host, port, "/" ^ uri)

let read_stream_full = function
  | None -> Lwt.return ""
  | Some content ->
    let rec read_stream acc = function
      | Ocsigen_stream.Finished (None) -> Lwt.return acc
      | Ocsigen_stream.Finished (Some stream) -> Ocsigen_stream.next stream >>= read_stream acc
      | Ocsigen_stream.Cont (s, next_stream) -> Ocsigen_stream.next next_stream >>= read_stream (acc ^ s)
    in
    let st = Ocsigen_stream.get content in
    lwt s =
      lwt step = Ocsigen_stream.next st in
      read_stream "" step
    in
    lwt _ = Ocsigen_stream.finalize content `Success in
    Lwt.return s

let read_stream_bound ?max stream =
  let max = match max with None -> 100000 | Some max -> max in
  match stream with
  | None -> Lwt.return ""
  | Some stream ->
    let content = Ocsigen_stream.get stream in
    try_lwt
      lwt s = Ocsigen_stream.string_of_stream max content in
      lwt _ = Ocsigen_stream.finalize stream `Success in
      Lwt.return s
    with _ ->
      lwt _ = Ocsigen_stream.finalize stream `Success in
      Lwt.return ""

let write_to_disk file frame =
  match frame.frame_content with
  | None -> raise Not_found
  | Some stream ->
    lwt chan = Lwt_io.open_file Lwt_io.output file in
    let content = Ocsigen_stream.get stream in
    let rec read s =
      match_lwt Ocsigen_stream.next s with
      | Ocsigen_stream.Finished None -> Lwt.return frame.frame_header.Http_header.headers
      | Ocsigen_stream.Finished (Some s) -> read s
      | Ocsigen_stream.Cont (a,s) ->
	lwt _ = Lwt_io.write chan a in
	read s
    in
    lwt h = read content in
    lwt _ = Lwt_io.close chan in
    lwt _ = Ocsigen_stream.finalize stream `Success in
    Lwt.return h

let read_response ?max frame =
  lwt s = read_stream_bound ?max frame.frame_content in
  Lwt.return (s, frame.frame_header.Http_header.headers)

let rec do_safe_call ?max f =
  try_lwt
    f () >>= (read_response ?max)
  with
  | Ocsigen_http_com.Keepalive_timeout ->
    lwt _ = Lwt_unix.sleep 1. in
    do_safe_call f
  | exc -> fail exc

let rec do_safe_call_file file f =
  try_lwt
    f () >>= (write_to_disk file)
  with
  | Ocsigen_http_com.Keepalive_timeout ->
    lwt _ = Lwt_unix.sleep 1. in
    do_safe_call_file file f
  | exc -> fail exc

(***** GET REQUEST *****)

let get ?max ?https ?port ?(headers=[]) ~host ~uri () =
  let headers = generate_headers headers in
  do_safe_call ?max (Ocsigen_http_client.get ~headers ?https ?port ~host ~uri)

let rec get_safe ?(headers=[]) ~host ~uri () =
  try_lwt
    get ~headers ~host ~uri ()
  with _ -> Lwt_log.ign_info_f "request to %s failed trying in one second" uri ; Lwt_unix.sleep 1.0 >>= get_safe ~headers ~host ~uri

let get_url ?max ?headers url () =
  let (https, host, port, uri) = fragment_url url in
  get ?max ?https ?port ?headers ~host ~uri ()

let get_file ?(headers=[]) url file =
  let headers = generate_headers headers in
  let (https, host, port, uri) = fragment_url url in
  do_safe_call_file file (Ocsigen_http_client.get ~headers ?https ?port ~host ~uri)

(***** POST REQUEST *****)

let post_string ?https ?port ?(headers=[]) ~host ~uri ~content ?(content_type=("application","x-www-form-urlencoded")) () =
  let headers = generate_headers headers in
  let content = Netencoding.Url.mk_url_encoded_parameters content in
  do_safe_call (Ocsigen_http_client.post_string ?https ?port ~headers ~host ~uri ~content ~content_type)

let post_string_raw ?https ?port ?(headers=[]) ~host ~uri ~content ?(content_type=("application","x-www-form-urlencoded")) () =
  let headers = generate_headers headers in
  do_safe_call (Ocsigen_http_client.post_string ?https ?port ~headers ~host ~uri ~content ~content_type)

let post_string_url ?headers ?content_type ~content url () =
  let (https, host, port, uri) = fragment_url url in
  post_string ?https ?port ?headers ~host ~uri ~content ?content_type ()

let remove_newline =
  Pcre.replace ~rex:(Pcre.regexp "\n") ~templ:""

let base64 str =
  (* the encoder is consumed by its use, so we have to recreated *)
  let b64_encoder = Cryptokit.Base64.encode_multiline () in
  let encoded = Cryptokit.transform_string b64_encoder str in

  (* we want to retain the trailing '=' characters, but eliminate the
     newlines.  Unfortunately, [encode_compact] has neither. *)
  remove_newline encoded

(* Extra headers for basic authentication *)
let basic_auth login password =
  let str = login ^ ":" ^ password in
  let str64 = base64 str in
  [ "Authorization", "Basic " ^ str64 ]
