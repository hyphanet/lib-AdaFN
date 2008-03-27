 

with Agpl.Types;

package Agpl.Conversions is

   pragma Preelaborate;

   --  A two char string
   function From_Hex (C : Types.Hex_Character) return Character;
   function From_Hex (S : String) return String;

   function To_Hex (C : in Character) return Types.Hex_Character;
   function To_Hex (S : in String) return String;
   function To_Hex (I : in Natural; Length : in Natural := 0) return String;
   --  Return the hex representation, 0-left-padded to use at most Length chars

   function To_String (N : Integer) return String;

   --  Works as 'Img but removes leading/trailing spaces
   --  Performs rounding on floats
   function To_string (N        : Float;
                       Decimals : Natural := 2)
                       return     String;

   function To_string (N        : Long_Long_Float;
                       Decimals : Natural := 2)
                       return     String;

   generic
      type Real is digits <>;
   function To_Str (N        : Real;
                    Decimals : Natural := 2)
                    return     String;

   generic
      type Real is delta <> digits <>;
   function Fixed_To_Str (N        : Real;
                          Decimals : Natural := 2)
                          return     String;

   function Trim (This : in String) return String;
--   pragma Inline (Trim);

end Agpl.Conversions;
