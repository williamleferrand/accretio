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
    context.message_supervisor
      ~subject:"Reminder sent"
      ~content:[
        pcdata "I had to send a remind to message "; pcdata (string_of_int message) ; br () ;
      ]
      ()
  in
  lwt _ =
    context.reply_to
      ~message
      ~original_stage:true
      ~content:[ pcdata "Have you seen my previous message? Thanks!" ; br () ]
      ()
  in
  return `None

COMPONENT

remind__
