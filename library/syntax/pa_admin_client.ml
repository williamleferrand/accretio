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

open Pa_type_conv
open Pa_options
open Pa_tools

let () =
  add_generator_with_arg
    "vertex"
    options
    (fun options _ _ ->
       let _loc = Loc.ghost in
       <:str_item< >>)

let () =
  add_generator_with_arg
    "bin_io"
    options
    (fun options _ _ ->
       let _loc = Loc.ghost in
       <:str_item< >>)
