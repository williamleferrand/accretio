(* basics.ml *)

open Lwt

open Printf
open CalendarLib

open Api

open Eliom_content.Html5
open Eliom_content.Html5.D

open Message_parsers

let alert_supervisor context () =
  context.log_info "alerting supervisor, manual action is needed" ;
  lwt _ =
    context.message_supervisor
      ~subject:"Manual action is needed"
      ~content:[
        pcdata "Hi," ; br () ;
        br () ;
        pcdata "Please connect to your dashboard and check the society. You can also use this direct link:" ; br () ;
        br () ;
        Raw.a ~a:[ a_href (uri_of_string  (fun () -> context.direct_link)) ] [ pcdata context.direct_link ] ; br ();
        br () ;
      ]
      ()
  in
  return `None

COMPONENT

   *alert_supervisor
