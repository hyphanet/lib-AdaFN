with Agpl.Strings;

package body Adafn.Source is

   -----------------
   -- Upload_From --
   -----------------

   function Upload_From (This : Object) return String is
   begin
      if This.Kind = Directfromdisk then
         return "Direct";
      else
         return Agpl.Strings.Capitalize (This.Kind'Img);
      end if;
   end Upload_From;

end Adafn.Source;
