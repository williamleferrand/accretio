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

open Camlp4
open PreCast
open Ast

let translate _loc = function
  | <:ctyp< Edges.t >> -> <:ctyp< EdgesInternal.t >>
  | <:ctyp< edges >> -> <:ctyp< EdgesInternal.t >>
  | _ as tp -> tp

let rec split_modules_ctyp field_name tp =
  match tp with
   | <:ctyp< $lid:lid$ >> -> [], lid
   | <:ctyp< edges $tp$ >> -> [ "Edges_"^field_name ], "t"
   | <:ctyp< $uid:m1$ . $lid:lid$ >> -> [m1], lid
   | <:ctyp< $uid:m1$ . $uid:m2$ . $lid:lid$ >> -> ([m1;m2], lid)
   | _ -> failwith "ctype not handled"

let rec wrap_with_modules _loc expr =
  function
  | [] -> expr
  | m :: modules ->
    let el1 = <:expr< $uid:m$ >> in
    let el2 = wrap_with_modules _loc expr modules in
    <:expr< $el1$ . $el2$ >>

let get_module_name _loc =
  Filename.chop_extension (Filename.basename (Loc.file_name _loc))
