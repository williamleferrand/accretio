(*
 * children_schoolbus
 *
 * this playbook organizes a preschool on wheels by proposing activities, booking a bus
 * and organizing pickups/dropoffs
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
open Toolbox

open Ys_uid

let author = "william@accret.io"
let name = "Children schoolbus"
let description = "this playbook organizes a preschool on wheels by proposing activities, booking a bus and organizing pickups/dropoffs"
let version = 0
let tags = ""

PLAYBOOK

   #import core_join_request


PROPERTIES
  - "Your duties", "None! All you have is to show up on time with your child at the pickup spot."
