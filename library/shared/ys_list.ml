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

let flatten_lwt l =
  return
    (List.fold_left
       (fun acc ->
          function
          | None -> acc
          | Some v -> v :: acc)
       []
       l)

let rec take n l =
  if n < 1 then
    []
  else
    match l with
    [] -> []
    | t ::q -> t :: (take (n-1) q)

let take_lwt f n l =
  let rec aux rem =
    function
    | [] -> return []
    | _ when rem <= 0 -> return []
    | t::q ->
      lwt tl = aux (rem-1) q in
      lwt hd = f t in
      return (hd :: tl)
  in
  aux n l

let take_option f n l =
  let rec aux rem =
    function
    | [] -> return []
    | _ when rem <= 0 -> return []
    | t::q ->
      match_lwt f t with
      | None -> aux rem q
      | Some hd ->
        lwt tl = aux (rem-1) q in
        return (hd :: tl)
  in
  aux n l

let find f l =
  try
    Some (List.find f l)
  with Not_found -> None

let rec dup e = function
    n when n < 1 -> []
  | n -> e :: (dup e (n - 1))

let rec roll f = function
  | n when n < 1 -> []
  | n -> f n :: roll f (n - 1)


module StringSet = Set.Make(String)

let uniq l =
  StringSet.elements
    (List.fold_left (fun acc e -> StringSet.add e acc) StringSet.empty l)

let merge l1 l2 =
  uniq (l1 @ l2)

let remove l1 l2 =
  let to_be_removed = List.fold_left (fun acc e -> StringSet.add e acc) StringSet.empty l2 in
  let to_be_kept = List.fold_left (fun acc e -> if StringSet.mem e to_be_removed then acc else StringSet.add e acc) StringSet.empty l1 in
  StringSet.elements to_be_kept

let concat = String.concat " "
