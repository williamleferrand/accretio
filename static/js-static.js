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
    script.src = url + "&sensor=false&callback=ys_external.__load_callback";
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
function youtube_parser(url){
    var regExp = /^.*((youtu.be\/)|(v\/)|(\/u\/\w\/)|(embed\/)|(watch\?))\??v?=?([^#\&\?]*).*/;
    var match = url.match(regExp);
    if (match&&match[7].length==11){
        return match[7];
    } else{
        return
    }
}

var autolinkOptions = { twitter: false }

autolinkOptions.replaceFn = function( autolinker, match ) {
          console.log( "href = ", match.getAnchorHref() );
          console.log( "text = ", match.getAnchorText() );

          switch( match.getType() ) {
          case 'url' :
              console.log( "url: ", match.getUrl() );

              if( match.getUrl().indexOf( 'youtube.com' ) === -1
                  || match.getUrl().indexOf( 'youtu.be' ) === -1  ) {

                  var uid = youtube_parser( match.getUrl() )
                  if (uid == null) return ;

                  var tag = new Autolinker.HtmlTag( {
                      tagName : 'iframe',
                      attrs   : { 'type': 'text/html',
                                  'src': '//www.youtube.com/embed/' + uid + '?modestbranding=1&showinfo=0&autohide=1',
                                  'frameborder': '0' }
                  } );

                  return tag;

              } else {
                  return true;  // let Autolinker perform its normal anchor tag replacement
              }
          default:
              return;
          }
      }

var defaultAutolinkOptions = { twitter: false }
var hoverFix = function()
{
    var el = this;
    var par = el.parentNode;
    var next = el.nextSibling;
    par.removeChild(el);
    par.insertBefore(el, next);
}
