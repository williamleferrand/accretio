open Lwt
open Api

open Eliom_content.Html5
open Eliom_content.Html5.D

let author = "william@accret.io"
let name = "Breadmaking circle"
let description = "Baking bread for the neighbors"

let version = 0

(* the individual stages ****************************************************)

let check_if_baker_is_available_this_week context () =
  lwt _ =
    context.message_supervisor
      ~subject:"Are you available this week?"
      ~content:[
        pcdata "Hi," ; br () ;
        br () ;
        pcdata "Are you available to bake bread this week?"
      ] in
  return `None

let decode_baker_reply context message =
  return `None

(* the graph ****************************************************************)

PLAYBOOK

  check_if_baker_is_available_this_week ~> `Message of int ~> decode_baker_reply


CRON check_if_baker_is_available_this_week "0 0 * * 1 *"
