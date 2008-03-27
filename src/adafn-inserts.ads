with Adafn.Key;

package Adafn.Inserts is

--   pragma Preelaborate;

   type Uri (Kind : Key.Kinds := Key.Chk) is private;

   function Create (Kind : Key.Kinds;
                    Uri  : Key.Object := Key.Null_Object) return Inserts.Uri;

   function Create (Uri : Key.Object) return Inserts.Uri;
   --  For non CHK@ inserts

   function Image (This : Uri) return String;

   function Get_Uri (This : Uri) return Key.Object;

   function Has_Uri (This : Uri) return Boolean;

   procedure Set_Uri (This : in out Uri; Uri : Key.Object);

private

   type Uri (Kind : Key.Kinds := Key.Chk) is record
      Uri  : Key.Object := Key.Null_Object;
      case Kind is
         when Key.Chk =>
            Has_Uri : Boolean := False;
         when others  =>
            null;
      end case;
   end record;

end Adafn.Inserts;
