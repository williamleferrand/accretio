(*
 * children_schoolbus_transporation
 *
 * this playbook figures out the transportation solution. it currently manually
 * asks for a quote, but later it could figure out the bus availability, etc
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
let name = "Children schoolbus transportation"
let description = "this playbook figures out the transporation solution"
let version = 0
let tags = ""

(* the stages *****************************************************************)

let desk context message =
  lwt content = context.get_message_content ~message in
  try_lwt
    let quote_request = Yojson_quote_request.from_string content in
    return (`ComputeQuote quote_request)
  with exn ->
    context.log_error ~exn "couldn't respond to the quote request" ;
    lwt _ =
      context.forward_to_supervisor
        ~message
        ~subject:"Couldn't extract quote request"
        ~content:[]
        ()
    in
    lwt _ =
      context.reply_to
        ~message
        ~content:[ pcdata "ERROR" ]
        ()
    in
    return `None

let compute_quote context (quote_request: quote_request) =
  context.log_info "got quote request" ;
  let empty_quote =
    {
      reference = quote_request.reference ;
      number_of_seats = 0  ;
      cost = 0.0 ;
      currency = "USD"
    }
  in
  lwt _ =
    context.message_supervisor
      ~subject:"New quote request"
      ~content:[
        pcdata "Greetings," ; br () ;
        br () ;
        pcdata "Here is a new quote request:" ; br () ;
        br () ;
        pcdata (Yojson_quote_request.to_string quote_request) ;
        br () ;
        br () ;
        pcdata "Could you fill the JSON below with your response?" ; br () ;
        br () ;
        pcdata (Yojson_quote.to_string empty_quote) ; br () ;
        br () ;
        pcdata "Thanks!"
      ]
      ()
  in
  return `None

let extract_quote_and_send_it_back context message =
  return `None

(* the playbook ***************************************************************)

PLAYBOOK

-desk ~> `ComputeQuote of quote_request ~> compute_quote ~> `Message of email ~> extract_quote_and_send_it_back

PROPERTIES
  - "Your duties", "None"
