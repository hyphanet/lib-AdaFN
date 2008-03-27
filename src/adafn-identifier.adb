with Agpl.Random;

with Ada.Characters.Handling;

package body Adafn.Identifier is

   function "<" (L, R : Object) return Boolean is
   begin
      return (+L.Id) < (+R.Id);
   end "<";

   --------------
   -- Generate --
   --------------

   function Generate (Prefix : String := "") return Object is
      Id : String (1 .. 20);
   begin
      for I in Id'Range loop
         Id (I) :=
           Character'Val
             (Agpl.Random.Get_Integer
                  (Character'Pos ('a'),
                   Character'Pos ('z')));
      end loop;

      return (Id => +Sanitize (Prefix & Id));
   end Generate;

   -----------
   -- Image --
   -----------

   function Image (This : Object) return String is
   begin
      return +This.Id;
   end Image;

   -----------
   -- Value --
   -----------

   function Value (This : String) return Object is
   begin
      return (Id => +Sanitize (This));
   end Value;

   --------------
   -- Sanitize --
   --------------

   function Sanitize (This : String) return String is
      use Ada.Characters.Handling;
      Sane : String := This;
   begin
      for I in Sane'Range loop
         if not Is_Alphanumeric (Sane (I)) then
            Sane (I) := '-';
         end if;
      end loop;

      return Sane;
   end Sanitize;

end Adafn.Identifier;
