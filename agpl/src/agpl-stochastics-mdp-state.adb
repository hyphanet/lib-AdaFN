with Ada.Strings.Hash;

package body Agpl.Stochastics.Mdp.State is

   --------------
   -- Distance --
   --------------

   function Distance (This : in Object) return Distances
   is
      pragma Unreferenced (This);
   begin
      return 0.0;
   end Distance;

   ---------
   -- "<" --
   ---------

   function "<" (L, R : in Object'Class) return Boolean is
      LD : constant Distances := L.Distance;
      RD : constant Distances := R.Distance;
   begin
      if LD = RD then
         return L.Get_Id < R.Get_Id;
      else
         return LD < RD;
      end if;
   end "<";

   ----------
   -- Hash --
   ----------

   function Hash (Id : in Object_Id) return Ada.Containers.Hash_Type is
   begin
      return Ada.Strings.Hash (String (Id));
   end Hash;

   ---------------
   -- To_String --
   ---------------

   function To_String (This : in Object) return String is
   begin
      return String (Get_Id (Object'Class (This)));
   end To_String;

end Agpl.Stochastics.Mdp.State;
