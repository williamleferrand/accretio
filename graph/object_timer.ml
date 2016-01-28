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


(* I'm sorry that I need this object, but pa_vertex already has the logic
   to perform full text search on the labeled edges so it feels right to
   leverage this feature *)

open Lwt
open Bin_prot.Std

open Ys_default
open Ys_types
open Ys_uid
open Ys_executor

type t = {

  uid : uid ;

  created_on : timestamp ;
  society : uid ;

  call : call ;

} with vertex
  (
    {
      required = [ society ; call ] ;
    }
  )
