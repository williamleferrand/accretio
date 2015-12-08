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


open Str
open Lwt
open Bin_prot.Std

type uid = int with bin_io
type uids = int list with bin_io

(* uids ***********************************************************************)

let to_string = string_of_int
let to_string_padded = Printf.sprintf "%016d"
let of_string = int_of_string

let is_valid uid = uid <> 0

let extract_from_key =
  function
  | "" -> 0
  | _ as s ->
    match split (regexp "[_]+") s with
    | uid :: _ -> int_of_string uid
    | _ -> failwith "can't read the uid from the key"

let uids_of_string data =
  let len = String.length data in
  let buf = Bin_prot.Common.create_buf len in
  Bin_prot.Common.blit_string_buf data buf ~len ;
  bin_read_uids buf ~pos_ref:(ref 0)

let uids_to_string uids =
  let size = bin_size_uids uids in
  let buf = Bin_prot.Common.create_buf size in
  let s = Bytes.create size in
  ignore(bin_write_uids buf uids ~pos:0);
  Bin_prot.Common.blit_buf_string buf s size ;
  Bytes.to_string s

(* edges **********************************************************************)

type 'a edges = ('a * uid) list with bin_io

module Edges =
  struct

    let remove uid =
      List.filter (fun v -> snd v <> uid)

    let mem uid edges =
      List.mem uid (List.map snd edges)

    let cardinal edges =
      List.length edges

    let cardinal_filtered filter edges =
      List.length (List.filter (fun edge -> filter (fst edge)) edges)

    let uids edges =
      List.map snd edges

    (* merge two list of edges, keeping the one we already have *)
    (* arg, this is a real bug!! *)
    let add_unique v edges = v :: (remove (snd v) edges)
    let add v edges = v :: edges (* we'll have to revisit this one day *)

    (* use these functions *only* if you know what you are doing. some of them
       erase their tags and might lead to horrible bugs *)

    let map_s f =
      Lwt_list.map_s (fun edge -> f (snd edge))

    let map_p f =
      Lwt_list.map_p (fun edge -> f (snd edge))


(*
    let add_unique_all_keep edges_to_merge edges =
      (* TODO: do something efficient here, probably involving sets *)
      List.fold_left
        (fun acc v ->
           let uid' = snd v in
           match List.exists (function (_, uid) -> uid = uid') acc with
             true -> acc
           | false -> v :: acc)
        edges
        edges_to_merge *)

    let rec find uid = function
        [] -> None
      | (label, uid') :: _ when uid' = uid -> Some label
      | _ :: q -> find uid q

    let rec find_by_label label = function
        [] -> None
      | (label', uid) :: _ when label' = label -> Some uid
      | _ :: q -> find_by_label label q

    let rec find_all uid = function
        [] -> []
      | (label, uid') :: q when uid' = uid -> label :: (find_all uid q)
      | _ :: q -> find_all uid q

  end


module UidSet = Set.Make(struct type t = int let compare = Pervasives.compare end)
module UidMap = Map.Make(struct type t = int let compare = Pervasives.compare end)

let diff ~filter excludes l =
  let excludes = List.fold_left (fun acc e -> UidSet.add e acc) UidSet.empty excludes in
  List.fold_left
    (fun acc (tag, uid) ->
       if (filter tag && not (UidSet.mem uid excludes)) then uid :: acc else acc)
    []
    l

let merge l1 l2 =
  let s1 = List.fold_left (fun acc e -> UidSet.add e acc) UidSet.empty l1 in
  let s2 = List.fold_left (fun acc e -> UidSet.add e acc) s1 l2 in
  UidSet.elements s2

let set_of_edges edges =
  List.fold_left
    (fun acc (_, uid) -> UidSet.add uid acc)
    UidSet.empty
    edges

type uid_option = int option with bin_io

let of_list list =
  List.fold_left (fun acc uid -> UidSet.add uid acc) UidSet.empty list
