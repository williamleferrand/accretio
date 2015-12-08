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


(* state **********************************************************************)

type state =
    Active
  | Archived
  | Ghost
  | Suspended of string with bin_io, default_value(Active)

(* settings *******************************************************************)

type settings =
  {
    email_notify_batch_emails : bool ;
    show_introduction: bool;
  } with bin_io, default_value({
    email_notify_batch_emails = false ;
    show_introduction = true ;
  })

(* rights *********************************************************************)

type rights = Default | Admin with bin_io, default_value(Default)

(* the user object ************************************************************)

type strings = string list with bin_io, default_value ([])

type stripe = { stripe_customer: string; stripe_last4: string } with bin_io
type stripe_option = stripe option with bin_io, default_value (None)

(* a simple type for surveys **************************************************)

type t = {

  uid : uid ;
  siblings : [ `Ghost ] edges ;
  created_on : timestamp ;

  name : string ;
  profile_picture : uid_option ;
  album : [ `ProfilePicture ] edges ;

  preferred_email : string ;
  emails : strings ;

  state : state ;
  authentication : Ys_authentication.t ;
  rights : rights ;

  settings : settings ;

  (* recovery *)
  recovery_token : string ;
  recovery_token_expiration_timestamp : float ;

  (* batch emails marker *)
  last_timeline_batch_mailing_marker : int64 ;

  (* reviews *)
  reviews : [ `ThanksFromProject of uid | `ThanksFromMember | `ThanksForCohort of uid ] edges ;

  statement : string ;

  stripe : stripe_option ;

  panels : [ `Panel ] edges ;

  shunts : strings ;

  (* the context stuff *)
  societies : [ `Society ] edges ;
  skills : string ;

} with vertex
  (
    {
      aliases = [ `Strings emails ;
                  `Strings shunts ;
                  `PlainText name ;
                  `PlainText skills ;
                  `String recovery_token ] ;
      required = [ preferred_email ; emails ] ;
      uniques = [ emails ; recovery_token ; shunts ] ;
      builders = [
        (emails,
         (fun uid ->
            get_name uid
            >>= fun name ->
            return [ "please update your email, or we will use " ^ name ]))
      ]
    }
  )
