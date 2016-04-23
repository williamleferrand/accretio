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

(* some keys ******************************************************************)

let key_original_message = "original-message"

(* the stages *****************************************************************)

let desk context () =
  return `None

let extract_quote_request context message =
  lwt content = context.get_message_content ~message in
  try_lwt
    let quote_request = Yojson_quote_request.from_string content in
    return (`ComputeQuote (message, quote_request))
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

let compute_quote context ((message, quote_request): (int * quote_request)) =
  context.log_info "got quote request: %s %s" quote_request.reference quote_request.comment ;
  let empty_quote =
    {
      reference = quote_request.reference ;
      number_of_seats = 0  ;
      cost = 0.0 ;
      currency = "USD" ;
      description = "" ;
    }
  in
  lwt _ =
    context.message_supervisor
      ~subject:"New quote request"
      ~data:[ key_original_message, string_of_int message ]
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
  match_lwt context.get_message_data ~message ~key:key_original_message with
    None ->
    lwt _ =
      context.reply_to
        ~message
        ~content:[ pcdata "Couldn't find the original message" ]
        ()
    in
    return `None
  | Some message ->
    let message = int_of_string message in
    (* I'm going to hardcode the quotes for tonight *)
    let sfminibus =
      {
        reference = "" ;
        number_of_seats = 21  ;
        cost = 900.0 ;
        currency = "USD" ;
        description = "From SFMinibus, 21 seats bus with seatbelts" ;
      }
    in
    let coach21_23 =
      {
        reference = "" ;
        number_of_seats = 23  ;
        cost = 800.0 ;
        currency = "USD" ;
        description = "From Coach21, 23 seats bus without seatbelts" ;
      }
    in
    let coach21_56 =
      {
        reference = "" ;
        number_of_seats = 56  ;
        cost = 1100.0 ;
        currency = "USD" ;
        description = "From Coach21, 56 seats bus with seatbelts" ;
      }
    in
    let quotes = [ sfminibus ; coach21_23 ; coach21_56 ] in
    lwt _ =
      context.reply_to
        ~message
        ~content:[ Unsafe.data (Yojson_quotes.to_string quotes) ]
        ()
    in
    return `None

(* the playbook ***************************************************************)

PLAYBOOK

*desk<forward> ~> `Message of email ~> extract_quote_request ~> `ComputeQuote of (int * quote_request) ~> compute_quote ~> `Message of email ~> extract_quote_and_send_it_back

PROPERTIES
  - "Your duties", "None"
