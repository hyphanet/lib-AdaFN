 

--  Types of general use across Agpl

with Interfaces;

package Agpl.Types is

   pragma Pure;

   type Natural_Array is array (Positive range <>) of Natural;

   subtype Hex_Character is String (1 .. 2);

   type Unsigned_8 is new Interfaces.Unsigned_8;

   type Rgb_triplet is record
      R, G, B : Unsigned_8;
   end record;

   type Rgb_array is array (Integer range <>) of Rgb_triplet;

   type Utf8String is new String;

   --  In Stream_Elements/Second
   type Data_Rate is new Float;

end Agpl.Types;
