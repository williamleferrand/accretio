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

open Lwt

open Sociaml_oauth_client_v1_0a
open Sociaml_oauth_client_posix

module Client = Client.Make(Clock)(Cohttp_lwt_unix.Client)(MAC_SHA1)(Random)

open Client
open Sociaml_oauth_client.Result


let get_business business =
  let access_token =
    {
      consumer_key = Ys_config.get_string Ys_config.yelp_consumer_key ;
      consumer_secret = Ys_config.get_string Ys_config.yelp_consumer_secret ;
      token = Ys_config.get_string Ys_config.yelp_token ;
      token_secret = Ys_config.get_string Ys_config.yelp_token_secret ;
    }
  in
  match_lwt do_get_request
    ~uri:(Uri.of_string ("https://api.yelp.com/v2/business/"^business))
    ~access_token
    () with
  | Error _ -> return_none
  | Ok result -> return (Some result)
