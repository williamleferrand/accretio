(*
 * core - remind
 *
 * william@accret.io
 *
 *)

(* this component implements Api.Stage.remind__ *)

open Lwt

open Printf
open CalendarLib

open Api

open Eliom_content.Html5
open Eliom_content.Html5.D

open Message_parsers

let remind__ context message =
  context.log_info "time to remind message %d" message ;
  lwt _ =
    context.reply_to
      ~message
      ~preserve_origin:true
      ~content:[ pcdata "I hope all is well. Have you seen my previous message? Thanks!" ; br () ]
      ()
  in
  return `None

COMPONENT

remind__