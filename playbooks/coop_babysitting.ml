(*
 * coop_babysitting
 *
 * this playbook helps parents taking turns at babysitting
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
let name = "Coop babysitting"
let description = "This playbook helps parents taking turns at babysitting"

let version = 0


(* the playbook ***************************************************************)

PLAYBOOK

   #import core_join_request
   #import core_invite
