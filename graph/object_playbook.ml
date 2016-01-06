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
open Bin_prot.Std

open Ys_default
open Ys_types
open Ys_uid

type parameter =
  {
    label : string ;
    key : string ;
  } with bin_io

type parameters = parameter list with bin_io, default_value([])

type t = {

  uid : uid ;

  created_on : timestamp ;
  owner : uid ;
  scope : Ys_scope.t ;

  hash : string ;

  name : string ;
  description : string ;

  societies : [ `Society ] edges ;

  parameters : parameters ;

  (* old fields *)
  source : string ;

} with vertex
  (
    {
      aliases = [ `PlainText description ; `PlainText name ; `String hash ] ;
      required = [ owner ; name ; description ; hash ; scope ] ;
      uniques = [ hash ] ;
      builders = [ (scope, (fun _ -> return Ys_scope.Private)) ] ;
    }
  )
