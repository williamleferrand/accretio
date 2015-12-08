(*
 * Accretio is an API, a sandbox and a runtime for social playbooks
 *
 * Copyright (C) 2015 William Le Ferrand
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)


type 'a cache = {
  config : (string, 'a) Hashtbl.t ;
  get : string -> 'a ;
  set : string -> 'a -> unit ;
  mem : string -> bool
}

let create_cache () =
  let config = Hashtbl.create 10 in
  {
    config ;
    set = Hashtbl.replace config ;
    get = Hashtbl.find config ;
    mem = Hashtbl.mem config ;
  }

let string_cache: string cache = create_cache()
let int_cache: int cache = create_cache()
let float_cache: float cache = create_cache()

let set key = function
  | `String v -> string_cache.set key v
  | `Int v -> int_cache.set key v
  | `Float v -> float_cache.set key v

let wrap f key =
  try
    f key
  with Not_found -> Lwt_log.ign_error_f "%s not found" key ; raise Not_found

let wrap_with_default f key default =
  try
    f key
  with Not_found -> default

let get_string = wrap string_cache.get
let get_int = wrap int_cache.get
let get_float = wrap float_cache.get

let get_int_with_default = wrap_with_default int_cache.get

let if_prod if_prod if_not_prod =
  match get_string "dep" with
    "prod" -> if_prod ()
  | _ -> if_not_prod ()

let db_root_dir = "db-root-dir"
let cache_size = "cache-size"
let cache_size_aliases = "cache-size-aliases"
let private_key = "private-key"
let dep = "dep"
let aws_access_key_id = "aws-access-key-id"
let aws_secret_access_key = "aws-secret-access-key"
let email_feedback = "email-feedback"
let sphinx_pool_size = "sphinx-pool-size"
let recovery_token_timeout = "recovery-token-timeout"
let default_debate_duration = "default-debate-duration"
let mixpanel_id = "mixpanel-id"
let agora_page_size = "agora-page-size"
let suggestion_batch_mailing_hour = "suggestion-batch-mailing-hour"
let check_pool_expiration_period = "check-pool-expiration-period"

let fb_app_id = "fb-app-id"
let upload_directory = "upload-directory"
let stripe_publishable_key = "stripe-publishable-key"
let stripe_secret_key = "stripe-secret-key"
let twilio_sid = "twilio-sid"
let twilio_auth_token = "twilio-auth-token"

let decision_reminder_period_in_hours = "decision-reminder-period-in-hours"

let imap_host = "imap-host"
let imap_port = "imap-port"
let imap_user = "imap-user"
let imap_pass = "imap-pass"
let imap_mbox = "imap-mbox"
let imap_prefix = "imap-prefix"

let notify_monitoring_email = "notify-monitoring-email"

let required =
  [
    db_root_dir ;
    cache_size ;
    cache_size_aliases ;
    private_key ;
    dep ;
    aws_access_key_id ;
    aws_secret_access_key ;
    email_feedback ;
    sphinx_pool_size ;
    recovery_token_timeout ;
    default_debate_duration ;
    mixpanel_id ;
    agora_page_size ;
    suggestion_batch_mailing_hour ;
    fb_app_id ;
    upload_directory ;
    stripe_publishable_key ;
    stripe_secret_key ;
    twilio_sid ;
    twilio_auth_token ;
    check_pool_expiration_period ;
    decision_reminder_period_in_hours ;
    imap_host ;
    imap_port ;
    imap_user ;
    imap_pass ;
    imap_mbox ;
    notify_monitoring_email ;
  ]

(* other values are read from the xml configuration file ***********************)

open Simplexmlparser

let _ =
  let config = Eliom_config.get_config () in
  List.iter
    (function
      | Element (type_, attr, _) ->
        let key = List.assoc "key" attr
        and value = List.assoc "value" attr in
        (match type_ with
        | "int-param" -> set key (`Int (int_of_string value))
        | "string-param" -> set key (`String value)
        | "float-param" -> set key (`Float (float_of_string value))
        | _ -> ())
      | _ -> ())
    config

exception Missing_required_parameter of string

let _ =
  (* check that all the required keys are set *)
  List.map
    (fun key ->
       if not (string_cache.mem key)
       && not (int_cache.mem key)
       && not (float_cache.mem key) then
         raise (Missing_required_parameter key))
    required
