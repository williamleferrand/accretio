(*
 * ecd_homeschool
 *
 * this playbook implements an experimental curriculum for preschoolers
 *
 * william@accret.io
 *
 *)

open Lwt

open Printf
open CalendarLib
open Calendar.Precise

open Api

open Eliom_content.Html5
open Eliom_content.Html5.D

open Message_parsers
open Toolbox

let author = "william@accret.io"
let name = "At home preschool"
let description = "This playbook implements an at home preschool"

let version = 0

(* the curriculum *************************************************************)

let suggestions =
  [
    "Mandarin circle time" ;
    "English circle time" ;
    "Visit at the museum (Legion of Honor, DeYoung, ..)" ;
    "Group swimming lesson" ;
    "Early childhood class at the Zoo" ;
  ]

(* the stages *****************************************************************)

let schedule_week context () =
  lwt members = context.search_members "active" () in
  let run_id = new_run_id () in
  let suggestions = ul (List.map (fun suggestion -> li [ pcdata suggestion ]) suggestions) in
  lwt _ =
    Lwt_list.iter_s
      (fun member ->
         lwt _ =
           context.message_member
             ~member
             ~subject:"Children activities"
             ~data:(data_run_id run_id)
             ~content:[
               pcdata "Greetings," ; br () ;
               br () ;
               pcdata "I hope your week is off to a great start!" ; br () ;
               br () ;
               pcdata "Here are a few suggestions of activities that could be relevant for our children, could you tell me if you find anything appealing in the list? (No commitment at this point, I'm just testing the waters)"; br () ;
               suggestions ;
               pcdata "Also, do you want to suggest an activity?" ; br () ;
               br () ;
               pcdata "Looking forward to hearing from you!" ; br ()
             ]
             ()
         in
         return_unit)
      members
  in
  return `None



(* the flow *******************************************************************)

PLAYBOOK

   #import core_join_request
   #import core_invite


 *schedule_week
