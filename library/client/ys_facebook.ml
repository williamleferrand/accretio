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


open Eliom_content.Html5.D
open Lwt
open Js


let (>>>) f g = g f

type _method = GET | POST

(********* Like definition **********)

class type paging = object
  method next: js_string t opt readonly_prop
  method previous: js_string t opt readonly_prop
end

class type like = object
  method id: js_string t readonly_prop
  method name: js_string t readonly_prop
end

class type likes = object
  method data: like t js_array t readonly_prop
  method paging: paging t readonly_prop
end

(********** Photo definition ***********)

class type picture = object
  method height: int readonly_prop
  method width: int readonly_prop
  method source: js_string t readonly_prop
end

class type photo = object
  method id: js_string t readonly_prop
  method picture: js_string t readonly_prop
  method source: js_string t readonly_prop
  method height: int readonly_prop
  method width: int readonly_prop
  method images: picture t js_array t readonly_prop
  method link: js_string t readonly_prop
  method icon: js_string t readonly_prop
  method created_time: js_string t readonly_prop
  method position: int readonly_prop
  method updated_time: js_string t readonly_prop
  method likes: likes t readonly_prop
end

class type photos = object
  method data: photo t js_array t readonly_prop
end

class type album = object
  method id: js_string t readonly_prop
  method name: js_string t readonly_prop
  method link: js_string t readonly_prop
  method cover_photo: js_string t readonly_prop
  method privacy: js_string t readonly_prop
  method count: int readonly_prop
  method type_: js_string t readonly_prop
  method created_time: js_string t readonly_prop
  method updated_time: js_string t readonly_prop
  method can_upload: bool t readonly_prop
end

class type albums = object
  method data: album t js_array t readonly_prop
end

(********** User definition ***********)

class type hometown = object
  method id: js_string t readonly_prop
  method name: js_string Js.t readonly_prop
end

class type location = object
  method id: js_string Js.t readonly_prop
  method name: js_string Js.t readonly_prop
end

class type language = object
  method id: js_string Js.t readonly_prop
  method name: js_string Js.t readonly_prop
end

class type user = object
  method id: js_string Js.t readonly_prop
  method name: js_string Js.t readonly_prop
  method first_name_: js_string Js.t readonly_prop
  method middle_name_: js_string Js.t opt readonly_prop
  method last_name_: js_string Js.t readonly_prop
  method gender: js_string Js.t opt readonly_prop
  method locale: js_string Js.t readonly_prop
  method languages: language js_array Js.t readonly_prop
  method link: js_string Js.t readonly_prop
  method username: js_string Js.t opt readonly_prop
  method third_party_id_: js_string Js.t opt readonly_prop
  method timezone: js_string Js.t readonly_prop
  method updated_time_: js_string Js.t readonly_prop
  method verifier: bool Js.t readonly_prop
  method bio: js_string Js.t opt readonly_prop
  method birthday: js_string Js.t opt readonly_prop
  method email: js_string Js.t readonly_prop
  method hometown: hometown Js.t opt readonly_prop
  method location: location Js.t opt readonly_prop
  method political: js_string Js.t opt readonly_prop
end

(********* User action *********)

class type post = object
  method id : js_string Js.t opt readonly_prop
end

class type link = object
  method id : js_string Js.t opt readonly_prop
end

class type note = object
  method id : js_string Js.t opt readonly_prop
end

class type status = object
  method id : js_string Js.t opt readonly_prop
end

class type friend = object
  method id: js_string Js.t readonly_prop
  method name: js_string Js.t readonly_prop
end

class type friends = object
  method data: friend Js.t js_array Js.t readonly_prop
end

(********* End of user *********)

(********* FB.ui ****************)

class type ui = object
  method post_id_: js_string Js.t readonly_prop
end

(*********** End of FB.ui *************)

(*********** FB.dialog **********)

class type dialog = object
  method width : js_string Js.t prop
  method height : js_string Js.t prop
end

class type dialog_config = object
  method content : js_string Js.t Js.writeonly_prop
  method closeIcon : bool Js.t Js.writeonly_prop
  method onClose : unit Js.callback Js.writeonly_prop
  method visible : bool Js.t Js.writeonly_prop
end


let empty_dialog_config () = Js.Unsafe.coerce (jsnew Js.array_empty ())

(********** Other FB.ui ********)


(********** Global definition ********)

class type authResponse = object
  method accessToken: js_string Js.t readonly_prop
  method userID: js_string Js.t readonly_prop
  method expiresIn: int readonly_prop
end

class type login_response = object
  method authResponse: authResponse Js.t opt readonly_prop
  method status: js_string Js.t readonly_prop
end

class type event = object
  method subscribe: js_string Js.t -> ?cb:('a js_array Js.t -> unit) -> unit meth
end

class type fb = object
  method init: 'a -> unit meth
  method event: event Js.t readonly_prop
  method login: (login_response Js.t -> unit ) -> 'a -> unit meth
  method logout: (login_response Js.t -> unit ) -> unit meth
  method getLoginStatus : (login_response Js.t -> unit) -> unit meth
  method api: js_string Js.t -> js_string Js.t -> 'a -> ('a Js.t -> unit ) -> unit meth
  method ui: 'a -> ('a Js.t opt -> unit ) -> unit meth
end

let fb : fb Js.t option ref = ref None

let get_fb_value () =
  match !fb with
  | None -> failwith "The fb script has not been initialized"
  | Some fb -> fb

let set_fb_value value =
  fb := Some value;
  value

type params = { appId : string ; status : bool ; cookie : bool ; oauth : bool ; xfbml : bool }

let default = { appId = "" ; status = true ; cookie = true; oauth = false ; xfbml = true }

let string_of_method = function
  | GET -> "get"
  | POST -> "post"

let empty_object = Unsafe.eval_string "({})"

let jsobject_from_list l =
  let (id_l, value_l) = List.split l in
  let regexp = Regexp.regexp "{.+}" in
  List.fold_left2 (
    fun (nb, acc) id value ->
      match Regexp.string_match regexp value 0 with
      | Some _ ->
        (match nb = 1, nb = List.length l with
         | true, false -> (nb + 1, acc ^ (Printf.sprintf "({ %s : %s, " id value))
         | false, true -> (nb, acc ^ (Printf.sprintf "%s : %s })" id value))
         | true, true -> (nb, Printf.sprintf "({ %s : %s })" id value)
         | _ -> (nb + 1, acc ^ (Printf.sprintf "%s : %s, " id value)))
      | None ->
        (match nb = 1, nb = List.length l with
         | true, false -> (nb + 1, acc ^ (Printf.sprintf "({ %s : '%s', " id value))
         | false, true -> (nb, acc ^ (Printf.sprintf "%s : '%s' })" id value))
         | true, true -> (nb, Printf.sprintf "({ %s : '%s' })" id value)
         | _ -> (nb + 1, acc ^ (Printf.sprintf "%s : '%s', " id value)))
  ) (1,"") id_l value_l >>> fun (_, obj) -> obj

let jsarray_from_list l =
  List.fold_left (
    fun (nb, acc) (name, link) ->
      match nb = 1, nb = List.length l with
      | true, false -> (nb + 1, acc ^ (Printf.sprintf "[ { name : '%s', link : '%s' }, " name link))
      | false, true -> (nb, acc ^ (Printf.sprintf " { name : '%s', link : '%s' } ]" name link))
      | true, true -> (nb, acc ^ (Printf.sprintf "[ { name : '%s', link : '%s' } ]" name link))
      | _ -> (nb + 1, acc ^ (Printf.sprintf "{ name : '%s', link : '%s' }, " name link))
  ) (1, "") l >>> fun (_, obj) -> obj

let wrapper params f =

  let params_serialized =
    Printf.sprintf "({ appId : '%s', status : '%b', cookie : '%b', oauth: true, xfbml : '%b' })" params.appId params.status params.cookie params.xfbml in

  let init () =
    Js.Opt.case
      (Dom_html.document ## getElementById (Js.string "fb-root"))
      (fun _ -> Ys_log.error "fb root not set")
      (fun _ ->
         let script_list = Dom_html.document##getElementsByTagName (Js.string "script") in
         Js.Opt.iter (script_list##item (0)) (fun script ->
             Js.Opt.iter (script##parentNode) (fun head ->
                 let fb = set_fb_value Js.Unsafe.global##_FB in
                 fb##init (Unsafe.eval_string (params_serialized));
                 f fb
               )
           )
      )
  in

  let load_sdk () =
    Js.Opt.case (Dom_html.document ## getElementById (Js.string "fb-root")) (fun _ -> Ys_log.error "fb root not set")
      (fun _ ->
         let script_list = Dom_html.document##getElementsByTagName (Js.string "script") in
         Js.Opt.iter (script_list##item (0)) (fun script ->
             Js.Opt.iter (script##parentNode) (fun head ->
                 let fb_js = Dom_html.createScript Dom_html.document in
                 fb_js##id <- Js.string "facebook-jssdk" ;
                 fb_js##src <- Js.string "//connect.facebook.net/en_US/all.js" ;
                 (Obj.magic fb_js)##async <- Js._true ;
                 Dom.insertBefore head fb_js (script_list##item (0)) ;
               )
           )
      )
  in

  match !fb with
  | None -> ignore (Unsafe.global##fbAsyncInit <- init) ; load_sdk ()
  | Some fb -> (f fb)

(* API *)

let load handle =
  wrapper
    handle
    (fun _ -> ())

let api handle path ?(meth=GET) ?(params=[]) cb =
  wrapper
    handle
    (fun fb ->
       let meth = string_of_method meth in
       match params with
       | [] -> Js.Unsafe.meth_call fb ("api") [| Js.Unsafe.inject (Js.string path); Js.Unsafe.inject cb |]
       | _ -> Ys_log.info "calling API"; fb ## api ((Js.string path), (Js.string meth), (Unsafe.eval_string (jsobject_from_list params)), cb))

let put_link_user handle id ?(link="") ~message (cb : (link Js.t -> unit )) =
  let params =
    match link with
    | "" -> [("message", message)]
    | _ -> [("link", link); ("message", message)]
  in
  api handle (Printf.sprintf "/%s/feed" id) ~meth:POST ~params cb

(* UI *)

let ui handle params cb =
  wrapper
    handle
    (fun fb ->
       fb ## ui (Unsafe.eval_string (jsobject_from_list params), cb))

(* params is a string * string list => label * value list *)
let ui_post handle params (cb : (ui Js.t Js.opt -> unit)) =
  let params = ("method", "feed") :: params in
  ui handle params cb

let ui_send handle params (cb : (ui Js.t Js.opt -> unit)) =
  let params = ("method", "send") :: params in
  ui handle params cb

let ui_share handle params (cb : (ui Js.t Js.opt -> unit)) =
  let params = ("method", "share") :: params in
  ui handle params cb

(* login *)

let login ?(perms="") cb =
    let fb = get_fb_value () in
    match perms with
    | "" -> fb##login (cb, empty_object)
    | _ -> fb##login (cb, Unsafe.eval_string ("eval({scope: '" ^ perms ^ "'})"))

let login ~perms =
    let (res, w) = Lwt.task () in
    login ~perms (fun r ->
      if (Js.to_string r##status) = "connected" then
        begin
          Js.Opt.case (r##authResponse)
            (fun () -> Lwt.wakeup w None)
            (fun e -> Lwt.wakeup w (Some e))
        end else begin
        Lwt.wakeup w None
      end);
    res

let api path ?(meth=GET) ?(params=[]) cb =
    let fb = get_fb_value () in
    let meth = string_of_method meth in
    match params with
      | [] ->
        Js.Unsafe.meth_call fb ("api") [| Js.Unsafe.inject (Js.string path); Js.Unsafe.inject cb |]
                                       (* !fb ## api ((Js.string path), (Js.string meth), (Unsafe.eval_string (jsobject_from_list [])), cb) *)
      | _ -> fb ## api ((Js.string path), (Js.string meth), (Unsafe.eval_string (jsobject_from_list params)), cb)


 let get_user id =
    let (res, w) = Lwt.task () in
    api ("/" ^ id) ~meth:GET (Lwt.wakeup w) ;
    res
