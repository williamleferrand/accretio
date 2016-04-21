(*
 * children_schoolbus_planner
 *
 * this playbook plans the trips
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
open Ys_googlemaps_types

open Children_schoolbus_types

let author = "william@accret.io"
let name = "Children schoolbus planner"
let description = "this playbook plans the trips"
let version = 0
let tags = ""

(* this playbook expects to be connected to children_schoolbus_transportation
    and children_schoolbus_groups *)

(* some keys ******************************************************************)

(* some tags ******************************************************************)

(* the stages *****************************************************************)

(* the playbook ***************************************************************)

let validate_transporation context () =
  context.log_info "validate tranportation" ;
  match_lwt context.search_societies ~query:"tran*" () with
  | [] -> return `NoTransporationProvider
  | _ as providers ->
    let quote_request =
      {
        reference = "testquote" ;
        route = { legs = [] } ;
        comment = "this is a testing quote request"
      }
    in
    lwt _ =
      Lwt_list.iter_s
        (fun society ->
           context.log_info "messaging society %d" society ;
           lwt _ =
             context.message_society
               ~society
               ~stage:"desk" (* how am I supposed to know that? *)
               ~subject:"quote request"
               ~content:(Yojson_quote_request.to_string quote_request)
               ()
           in
           return_unit)
        providers
    in
    return `None

let no_transportation_provider context () =
  lwt _ =
    context.message_supervisor
      ~subject:"No transporation provider"
      ~content:[
        pcdata "Greetings," ; br () ;
        br () ;
        pcdata "This society isn't connected to a transporation provider"
      ]
      ()
  in
  return `None

PLAYBOOK

#import core_remind

*validate_transporation ~> `NoTransporationProvider ~> no_transportation_provider


PROPERTIES
  - "Your duties", "None"
