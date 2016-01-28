(*
 * flying club
 *
 * this playbook connects pilots and non pilots to organize sightseeing trips
 *
 * william@accret.io
 *
 *)

open Lwt

open Printf
open CalendarLib

open Api

open Eliom_content.Html5
open Eliom_content.Html5.D

open Message_parsers

let _ =
  CalendarLib.Time_Zone.change (CalendarLib.Time_Zone.UTC_Plus (-8))

let author = "william@accret.io"
let name = "Flying club"
let description = "This playbook connects pilots and non pilots to schedule sightseeing trips"

let version = 0


(* the playbook ***************************************************************)

PLAYBOOK

   #import core_join_request
   #import core_invite
