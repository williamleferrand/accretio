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
