/*
 * /ys/lib is an shared library for myrilion
 * Copyright (C) 2014 William Le Ferrand
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
 */

var ys_external = {}

function __retrieve_addr_component(res) {
    var location = {
	streetnb : null,
	street : null,
	city : null,
	state : null,
	country : null,
	postalcode : null,
	lat : null,
	lon :null,
	fullname:null,
    } ;
    var addr_cp = res.address_components;
    for (var i in addr_cp) {
	switch (addr_cp[i].types[0]) {
	case 'street_number':
            location.streetnb = addr_cp[i].long_name ;
            break;
	case 'route':
            location.street = addr_cp[i].long_name ;
            break;
	case "locality":
            location.city = addr_cp[i].long_name ;
            break;
	case "administrative_area_level_1":
            location.state = addr_cp[i].short_name ;
            break;
	case "country":
            location.country = addr_cp[i].long_name ;
            break;
	case "postal_code":
            location.postalcode = addr_cp[i].long_name ;
            break;
	case "bus_station":
            if (location.street == "") {
		location.street = addr_cp[i].long_name ;
            }
            break;
	}
    };
    if (res.geometry && res.geometry.location) {
	location.lat=res.geometry.location.lat();
    }
    if (res.geometry && res.geometry.location) {
	location.lon=res.geometry.location.lng();
    }
    if (res.formatted_address) {
	location.fullname = res.formatted_address;
    }
    return (location)
}

ys_external.initialize_map = function (url, lwt_wakener) {
    ys_external.__load_callback = lwt_wakener ;
    var script = document.createElement("script");
    script.type = "text/javascript";
    script.src = url + "&callback=ys_external.__load_callback";
    script.id = "google-map-script" ;
    document.body.appendChild(script);
}

ys_external.create_autocomplete = function (){
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


ys_external.geocode_address = function (address, lwt_wakener){
    var geocoder = new google.maps.Geocoder();
    geocoder.geocode({'address': address, 'region':null}, function(results, status) {
	if (status == google.maps.GeocoderStatus.OK && results.length > 0) {
            lwt_wakener(true);
        } else {
            lwt_wakener(false);
        }
    })
}
