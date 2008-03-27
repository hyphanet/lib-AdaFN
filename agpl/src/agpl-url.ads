
with Ada.Strings.Maps;

package Agpl.URL is

   use Ada;

   --  The general URL form as described in RFC2616 is:
   --
   --  http_URL = "http:" "//" host [ ":" port ] [ abs_path [ "?" query ]]
   --
   --  Note also that there are different RFC describing URL like the 2616 and
   --  1738 but they use different terminologies. Here we try to follow the
   --  names used in RFC2616 but we have implemented some extensions at the
   --  end of this package. For example the way Path and File are separated or
   --  the handling of user/password which is explicitly not allowed in the
   --  RFC but are used and supported in many browsers. Here are the extended
   --  URL supported:
   --
   --  http://username:password@www.here.com:80/dir1/dir2/xyz.html?p=8&x=doh
   --   |                            |       | |          |       |
   --   protocol                     host port path       file    parameters
   --
   --                                          <--  pathname  -->

   --
   --  URL Encoding and Decoding
   --

   Default_Encoding_Set   : constant Strings.Maps.Character_Set;
   Requiring_Encoding_Set : constant Strings.Maps.Character_Set;

   function Normalize (URL : in String) return String;
   --  Remove . and .. instances

   function Encode
     (Str          : in String;
      Encoding_Set : in Strings.Maps.Character_Set := Default_Encoding_Set)
      return String;
   --  Encode Str into a URL-safe form. Many characters are forbiden into an
   --  URL and needs to be encoded. A character is encoded by %XY where XY is
   --  the character's ASCII hexadecimal code. For example a space is encoded
   --  as %20.

   function Decode (Str : in String) return String;
   --  This is the oposite of Encode above

   function Requires_Encoding
     (Str : String;
      Set : Strings.Maps.Character_Set := Requiring_Encoding_Set)
      return Boolean;

private

   use type Ada.Strings.Maps.Character_Set;

   Default_Encoding_Set : constant Strings.Maps.Character_Set
     := Strings.Maps.To_Set
         (Span => (Low  => Character'Val (128),
                   High => Character'Val (Character'Pos (Character'Last))))
     or
       --  Strings.Maps.To_Set (";/?:@&=+$,<>#%""{}|\^[]` ");
     Strings.Maps.To_Set (";?:@&=+$,<>#%""{}|\^[]`' ");
   --  Removed '/' and added ''' which may be problematic

   --  Is as previous but without %, which doesnt trigger encoding
   Requiring_Encoding_Set : constant Strings.Maps.Character_Set
     := Strings.Maps.To_Set
         (Span => (Low  => Character'Val (128),
                   High => Character'Val (Character'Pos (Character'Last))))
     or
     Strings.Maps.To_Set (";?:@&=+$,<>#""{}|\^[]`' ");

end Agpl.URL;
