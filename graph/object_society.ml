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
open Ys_executor

type mode = Public | Private | Sandbox with bin_io

let edge_to_document =
  function
    `Member tags -> Some (Ys_list.concat tags)
  | `Candidate -> None

let timer_to_document =
  function
    `Label label -> Some label

type data = (string * string) list with bin_io, default_value([])

type slip =
  {
    received_on : timestamp ;
    read : bool ;
  } with bin_io

type stack = call list with bin_io, default_value([])
type history = (call * call option) list with bin_io, default_value([])

type int64s = int64 list with bin_io, default_value([])

type t = {

  uid : uid ;

  created_on : timestamp ;
  leader : uid ;

  name : string ;
  description : string ;

  mode : mode ;

  members : [ `Member of string list | `Candidate ] edges ;
  playbook : uid ;

  data : data ;

  inbox : [ `Message of slip ] edges ;
  outbox : [ `Message of slip ] edges ;

  stack : stack ;
  sidecar : stack ; (* to be written by the api method, before being merged into the main stack *)
  tombstones : int64s ;

  history : history ;

  timers : [ `Label of string ] edges ; (* these are fake edges *)
  shortlink : string ;

  blacklist : [ `Unregistered | `Banned ] edges ;
  payments : [ `Payment ] edges ;

} with vertex
  (
    {
      aliases = [ `String shortlink ; `PlainText name ; `PlainText description ; `PlainTextEdge timers timer_to_document ; `PlainTextEdge members edge_to_document ] ;
      required = [ shortlink ; leader ; name ; description ; mode ; playbook ] ;
      uniques = [ shortlink ]
    }
  )
