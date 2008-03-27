with Adafn.Key;

with Agpl.Ustrings; use Agpl.Ustrings;

package Adafn.Source is

--   pragma Preelaborate;

   type Kinds is (Direct, Directfromdisk, Disk, Redirect);
   --  Direct means data passed inline in Object
   --  Directfromdisk means to use Direct send for data in a supplied file
   --  Disk means to read from disk (node must be local!)
   --  Redirect is to a key
   --  Note that all correspond to the UploadFrom field in the
   --    ClientPutComplexDir, but DirectFromDisk, which requires tweaking.

   type Object (Kind : Kinds) is record
      Name : Ustring;
      case Kind is
         when Direct =>
            Data : Ustring;
         when Disk | DirectFromDisk =>
            Path : Ustring;
         when Redirect =>
            Uri  : Key.Object;
      end case;
   end record;

   function Upload_From (This : Object) return String;
   --  Returns the appropriate capitalized UploadFrom
   --  which is Kinds'Img, but for DirectFromDisk that gets transformed
   --  into Direct

end Adafn.Source;
