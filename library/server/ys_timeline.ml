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


(* all we need to keep is an ordered list of views *)

module Make (Ord: Set.OrderedType) =
struct

  module S = Set.Make(Ord)

  type u = {
    mutable set : S.t
  }

  let create () = {
    set = S.empty
  }

  let insert t e =
    t.set <- S.add e t.set

  let select t pivot size =
    let _, _, after = S.split pivot t.set in
    let l = ref [] in
    let rem = ref size in
    (try
       S.iter
         (fun e ->
            if !rem > 0 then
              (l := e :: !l ; decr rem)
            else
              failwith "done")
         after
     with _ -> ());
    !l

 let select_inverted t pivot size =
    let after, _, _ = S.split pivot t.set in
    let l = ref [] in
    let rem = ref size in
    (try
       S.iter
         (fun e ->
            if !rem > 0 then
              (l := e :: !l ; decr rem)
            else
              failwith "done")
         after
     with _ -> ());
    List.rev !l



end
