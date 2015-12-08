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


(* this is a very quick hack, we would really need to make it more robust *)

open Lwt


(*

{"first_page_uri": "/2010-04-01/Accounts/AC394972a4a36d5deade2166cde10b1bb5/Messages.json?To=%28415%29+915-2720&DateSent%3E=2015-03-23&PageSize=50&Page=0", "end": 0, "previous_page_uri": null, "messages": [{"sid": "SMfdc66287d424f254905666451aac3be9", "date_created": "Tue, 24 Mar 2015 04:27:19 +0000", "date_updated": "Tue, 24 Mar 2015 04:27:20 +0000", "date_sent": "Tue, 24 Mar 2015 04:27:20 +0000", "account_sid": "AC394972a4a36d5deade2166cde10b1bb5", "to": "+14159152720", "from": "+14156831484", "body": "Cool", "status": "received", "num_segments": "1", "num_media": "0", "direction": "inbound", "api_version": "2010-04-01", "price": null, "price_unit": "USD", "error_code": null, "error_message": null, "uri": "/2010-04-01/Accounts/AC394972a4a36d5deade2166cde10b1bb5/Messages/SMfdc66287d424f254905666451aac3be9.json", "subresource_uris": {"media": "/2010-04-01/Accounts/AC394972a4a36d5deade2166cde10b1bb5/Messages/SMfdc66287d424f254905666451aac3be9/Media.json"}}], "uri": "/2010-04-01/Accounts/AC394972a4a36d5deade2166cde10b1bb5/Messages.json?To=%28415%29+915-2720&DateSent%3E=2015-03-23&PageSize=50&Page=0", "page_size": 50, "start": 0, "next_page_uri": null, "num_pages": 1, "total": 1, "last_page_uri": "/2010-04-01/Accounts/AC394972a4a36d5deade2166cde10b1bb5/Messages.json?To=%28415%29+915-2720&DateSent%3E=2015-03-23&PageSize=50&Page=0", "page": 0}

*)

(*
{"sid": "SMfdc66287d424f254905666451aac3be9", "date_created": "Tue, 24 Mar 2015 04:27:19 +0000", "date_updated": "Tue, 24 Mar 2015 04:27:20 +0000", "date_sent": "Tue, 24 Mar 2015 04:27:20 +0000", "account_sid": "AC394972a4a36d5deade2166cde10b1bb5", "to": "+14159152720", "from": "+14156831484", "body": "Cool", "status": "received", "num_segments": "1", "num_media": "0", "direction": "inbound", "api_version": "2010-04-01", "price": null, "price_unit": "USD", "error_code": null, "error_message": null, "uri": "/2010-04-01/Accounts/AC394972a4a36d5deade2166cde10b1bb5/Messages/SMfdc66287d424f254905666451aac3be9.json", "subresource_uris": {"media": "/2010-04-01/Accounts/AC394972a4a36d5deade2166cde10b1bb5/Messages/SMfdc66287d424f254905666451aac3be9/Media.json"}}
*)

type message =
  {
    sid : string ;
    date_created : string ;
    date_updated : string ;
    date_sent : string ;
    account_sid : string ;
    _to : string ;
    from : string ;
    body : string ;
    status : string ;
    num_segments : string ;
    num_media : string ;
    direction : string ;
    api_version : string ;
    price : int option ;
    price_unit : string ;
    error_code : string option ;
    error_message : string option ;
    uri : string ;
  } deriving (Yojson)

type page = {
  first_page_uri : string ;
  _end: int ;
  previous_page_uri : string option ;
  messages : message list ;
  uri : string ;
  page_size : int ;
  start : int ;
  next_page_uri : string option ;
  num_pages : int ;
  total : int ;
  last_page_uri : string ;
  page : int ;
} deriving (Yojson)


let post_message ~from_number ~to_number ~body () =
  let sid = Ys_config.get_string Ys_config.twilio_sid in
  let auth_token = Ys_config.get_string Ys_config.twilio_auth_token in
  let headers = Ys_http.basic_auth sid auth_token in
  Ys_http.post_string
    ~https:true
    ~host:"api.twilio.com"
    ~headers
    ~uri:("/2010-04-01/Accounts/"^sid^"/Messages.json")
    ~content: [ "From", from_number ; "To", to_number ; "Body", body  ]
    ()
  >>= fun (reply, _) ->
  Printf.printf "reply is %s\n" reply ; flush stdout ;
  try_lwt
    let message = Yojson_message.from_string reply in
    Lwt.return (`Ok message)
  with e -> Lwt.return (`Error e)


let get_messages ~to_number ~date_sent () =
  let sid = Ys_config.get_string Ys_config.twilio_sid in
  let auth_token = Ys_config.get_string Ys_config.twilio_auth_token in
  let headers = Ys_http.basic_auth sid auth_token in
  let parameters =
    Netencoding.Url.mk_url_encoded_parameters
      [ "To", to_number ;
        "DateSent>", date_sent ]
  in
  Ys_http.get
    ~https:true
    ~host:"api.twilio.com"
    ~headers
    ~uri:(Printf.sprintf "/2010-04-01/Accounts/"^sid^"/Messages.json?" ^ parameters )
    ()
  >>= fun (reply, _) ->
  Printf.printf "reply is %s\n" reply ; flush stdout ;
  try_lwt
    let page = Yojson_page.from_string reply in
    Lwt.return (`Ok page)
  with e -> Lwt.return (`Error e)
