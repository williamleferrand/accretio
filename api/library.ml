(* some utilities *)

open Printf
open Lwt

open Api

let message_with_timeout context ~member ~subject ~content ~timeout =
  lwt _ = context.message_member ~member ~subject ~content in
  lwt _ = context.set_timer ~label:"email-timeout" ~duration:6 `TimedOut in
  return `None

let decode_message_yes_no context ~message =
  lwt content = context.get_message_content ~message in
  let lexbuf = Lexing.from_string content in
  match Parser.library_message_yes_no Lexer.library_message_yes_no lexbuf with
  | `Yes -> return (`Yes message)
  | `No -> return (`No message)
  | `Unknown -> return (`Unknown message)

let member_from_message context ~message =
  match_lwt context.get_message_senders ~message with
  | [] -> return `NoMemberFound
  | owner :: _ -> return (`Member owner)





(* some common CRON frequencies *)

let hourly = "0 * * * * *"
