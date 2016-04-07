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


(*  open Lwt

exception Address_not_found of string

let empty =
  { street_nb = None ; street = None ; city = None ;
    country = None; state = None ; postal_code = None ;
    lat = None ; lon = None ; fullname = None }

(************************
      Private function
   **************************)

let handle_exception exc sjs = Lwt.wakeup_exn exc (Address_not_found (Js.to_string sjs))

let _address_from_js res : address option =

  Js.Opt.case res
    (fun () -> None)
    (fun a ->
       let h f s = Balsa_option.map f (Js.Opt.to_option s) in
       let s = h Js.to_string in
       let f = h Js.to_float in
       Some {
         street_nb=s(a##streetnb);
         street=s(a##street);
         city=s(a##city);
         state=s(a##state);
         country=s(a##country);
         postal_code=s(a##postalcode);
         lon=f(a##lon);
         lat=f(a##lat);
         fullname= s(a##fullname);
       })

let _coord_from_js res =
  let a = Obj.magic res in
  let c x = float_of_string (Js.to_string x) in
  c (a##lat), c (a##lon)
(**************************
              Misc
     **************************)
let load_gmaps () =
  Js.Opt.case (Dom_html.document##getElementById (Js.string "google_map_script"))
    (fun _ ->
       let (res, w) = Lwt.task () in
       let url = "//maps.googleapis.com/maps/api/js" in
       let _ = Js.Unsafe.global##balsa##_initialize_map_(Js.string url,fun () -> Lwt.wakeup w ()) in
       res
    ) (fun _ -> Balsa_log.info "gmaps already initialized"; Lwt.return_unit)

(***********************************
            Address/Coord
   ***********************************)

let address_of_coord lat lng =
  let (res, w) = Lwt.task () in
  let _ = Js.Unsafe.global##balsa##_address_of_coord_(lat,lng, Lwt.wakeup w, handle_exception w) in
  res >>= fun l -> return  (_address_from_js l)


let coord_of_address address =
    let (res, w) = Lwt.task () in
    let _ = Js.Unsafe.global##balsa##_coord_of_address_ (Js.string address,Lwt.wakeup w,handle_exception w) in
    Lwt.map _coord_from_js res

  let update address =
    let address_string =
      (Balsa_option.default "" address.street_nb) ^ " " ^
        (Balsa_option.default "" address.street) ^ " " ^
        (Balsa_option.default "" address.city) ^ " " ^
        (Balsa_option.default "" address.country) in
    coord_of_address address_string
    >>= fun (lat, lon) -> Lwt.return { address with lat = Some lat; lon = Some lon }


  (***********************************
        Finding user position
   ***********************************)

  let coord_of_geo () =
    let (res, w) = Lwt.task () in
    let _ = Js.Unsafe.global##balsa##_coord_of_geo_(Lwt.wakeup w,handle_exception w)
    in
    Lwt.map _coord_from_js res

  (* let address_of_geo () = *)
  (*   lwt lat,lon = coord_of_geo () in *)
  (*   address_of_coord lat lon *)


  let create_autocp () =
    let f = Js.Unsafe.global##balsa##_create_autocomplete_() in
    let g max prefix =
      let (res, w) = Lwt.task () in
      let prefix = Js.string prefix in
      let _ = Js.Unsafe.fun_call f [| Js.Unsafe.inject prefix ; Js.Unsafe.inject  (fun r -> Lwt.wakeup w r) |] in
      lwt res = res in
      let res = Js.to_array res in
      let res = Array.to_list res in

      let res : address list =
        List.fold_left
          (fun acc e -> match _address_from_js e with None -> acc | Some s -> s :: acc)
          []
          (Balsa_list.take max res)
      in
      Lwt.return res in
    g


joo_global_object.balsa.create_autocomplete = function (){
    var geocoder = new google.maps.Geocoder();
    var f = function (prefix,cb) {
	geocoder.geocode({'address': prefix, 'region':null}, function(results, status) {
	    if (status == google.maps.GeocoderStatus.OK) {
                for (var i = 0; i < results.length; i++) {
		    results[i]=__retrieve_addr_component(results[i]);
                };
            }
            cb(results);
	})
    }
    return f;
}


*)

open Lwt
open Eliom_content.Html5.D

let ys_external = Js.Unsafe.variable "ys_external"

type address = {
  street_nb : string option;
  street : string option;
  city : string option;
  state : string option;
  country : string option;
  postal_code : string option;
  lat : float option;
  lon : float option;
  fullname : string option
}

let format_address address =
  match address.fullname with
    None -> None
  | Some name ->
    Some
      (div [
          pcdata name
        ])

let address_from_js res : address option =
  Js.Opt.case res
    (fun () -> None)
    (fun a ->
       let h f s = match Js.Opt.to_option s with None -> None | Some v -> Some (f v) in
       let s = h Js.to_string in
       let f = h Js.to_float in
       Some {
         street_nb = s(a##streetnb) ;
         street = s(a##street) ;
         city = s(a##city) ;
         state = s(a##state) ;
         country = s(a##country) ;
         postal_code = s(a##postalcode) ;
         lon = f(a##lon) ;
         lat = f(a##lat) ;
         fullname = s(a##fullname) ;
       })

let create_autocomplete () =
    let f = ys_external##_create_autocomplete_() in
    let g prefix max =
      let (res, w) = Lwt.task () in
      let prefix = Js.string prefix in
      let _ = Js.Unsafe.fun_call f [| Js.Unsafe.inject prefix ; Js.Unsafe.inject (fun r -> Lwt.wakeup w r) |] in
      lwt res = res in
      let res = Js.to_array res in
      let res = Array.to_list res in
      let res : address list =
        List.fold_left
          (fun acc e -> match address_from_js e with None -> acc | Some s -> s :: acc)
          []
          (res)
      in
      Lwt.return res in
    g

let geocode address =
  let (res, w) = Lwt.task () in
  let _ = ys_external##_geocode_address_(Js.string address, Js.Unsafe.inject (fun r -> Lwt.wakeup w (Js.to_bool r))) in
  res

let load () =
  Js.Opt.case
    (Dom_html.document##getElementById (Js.string "google-map-script"))
    (fun _ ->
       let (res, w) = Lwt.task () in
       let url = "//maps.googleapis.com/maps/api/js?js?v=3.exp&libraries=places" in
       let _ = ys_external##_initialize_map_(Js.string url, fun () -> Lwt.wakeup w ()) in
       res)
    (fun _ -> return_unit)


(* after the 10/15/14 rewrite ***********************************************************)

(* need the options too *)

(* the problem here is that you get the google logo + their CSS, and it is a pain to manage *)

open Js
open Eliom_content.Html5
open D

class type latlng = object
  method lat: number t meth
  method lng: number t meth
end

class type latlngbounds = object
  method getNorthEast: latlng t meth
  method getSouthWest: latlng t meth
end

class type placegeometry = object
  method location: latlng t readonly_prop
  method viewport: latlngbounds t opt readonly_prop
end

class type place = object
  method name : js_string t readonly_prop
  method geometry: placegeometry t readonly_prop
  method formatted_address_: js_string t readonly_prop
end

class type autocomplete = object
  method getPlace: place t optdef meth
end

let google () =
  Js.Unsafe.variable "google"

let create_autocomplete dom =
  let autocomplete_constr: (Dom_html.inputElement t -> Js.string_array -> autocomplete t) constr =
    (google())##maps##places##_Autocomplete
  in
  let options = Js.Unsafe.obj [||] in
  let types_ = jsnew array_length (1) in
  Js.array_set types_ 0 (Js.string "geocode") ;
  (* options##types <- types_ ; *)
  jsnew autocomplete_constr (To_dom.of_input dom, options)


let on_place_changed autocomplete callback : unit =
  (google())##maps##event##addListener(autocomplete, Js.string "place_changed", Js.wrap_callback callback)


(*

 google.maps.event.addListener(autocomplete, 'place_changed', function() {
    infowindow.close();
    marker.setVisible(false);
    var place = autocomplete.getPlace();
    if (!place.geometry) {
      return;
    }

*)
