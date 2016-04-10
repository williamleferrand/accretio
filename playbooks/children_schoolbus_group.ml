(*
 * children_schoolbus
 *
 * this playbook organizes field trips for a group of children
 *
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

open Children_schoolbus_types

let author = "william@accret.io"
let name = "Children schoolbus group"
let description = "this playbook organizes field trips for a group of children"
let version = 0
let tags = ""

(* the stages *****************************************************************)

let init__ context () =
  context.log_info "calling init for the society" ;
  lwt _ =
    context.message_supervisor
      ~subject:"Welcome"
      ~content:[
        pcdata "Greetings," ; br () ;
        br () ;
        pcdata "This society just got created." ; br () ;
      ]
      ()
  in
  return `None

(* the playbook ***************************************************************)

PLAYBOOK

*init__



PROPERTIES
  - "Your duties", "None"
