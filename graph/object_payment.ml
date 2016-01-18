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

type state = Pending | Failed of string | Paid with bin_io

type currency = USD with bin_io, default_value(USD)

let apply_stripe_fees amount =
  ceil (0.30 +. (amount *. 1.029))

type t = {

  uid : uid ;

  created_on : timestamp ;
  society : uid ;
  callback_success : Ys_executor.call_option ;
  callback_failure : Ys_executor.call_option ;

  label : string ;

  member : uid ;
  state : state ;
  evidence : Object_message.attachments ;

  shortlink : string ;

  amount : float ;
  currency : currency ;

} with vertex
  (
    {
      aliases = [ `String shortlink ] ;
      required = [ society ; member ; label ; shortlink ; amount ; state ; callback_success ; callback_failure ] ;
      uniques = [ shortlink ]
    }
  )
