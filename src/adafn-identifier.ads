private with Agpl.Types.Ustrings;

package Adafn.Identifier is

   --  pragma Preelaborate;

   type Object is private;

   function "<" (L, R : Object) return Boolean; pragma Inline ("<");

   function Generate (Prefix : String := "") return Object;
   --  Generate a random id.

   function Image (This : Object) return String;

   function Value (This : String) return Object;

   function Sanitize (This : String) return String;
   --  Sanitizes a string changing non-digit/char into -
   --  For use in Value

private

   use Agpl.Types.Ustrings;

   type Object is record
      Id : Ustring;
   end record;

end Adafn.Identifier;
