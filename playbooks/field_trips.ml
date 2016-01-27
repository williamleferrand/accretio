(*
 * field trips
 *
 * this playbook organizes field trips for preschoolers
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
let name = "Field trips"
let description = "This playbook organizes field trips for preschoolers"

let version = 0


PLAYBOOK

   #import core_join_request
   #import core_invite
