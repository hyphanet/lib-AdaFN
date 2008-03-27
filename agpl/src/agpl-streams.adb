 

with Ada.Unchecked_Conversion;

package body Agpl.Streams is

   -----------------------------
   -- To_Stream_Element_Array --
   -----------------------------

   function To_Stream_Element_Array (This : in String)
                                     return    Ada.Streams.Stream_Element_Array
   is
      subtype StringX is String (1 .. This'Length);
      subtype ArrayX  is Stream_Element_Array (1 .. This'Length);
      function To_Arr is new Ada.Unchecked_Conversion (StringX, ArrayX);
   begin
      return To_Arr (This);
   end To_Stream_Element_Array;

   ---------------
   -- To_string --
   ---------------

   function To_string (This : in Ada.Streams.Stream_element_array)
      return String
   is
      subtype StringX is String (1 .. This'Length);
      subtype ArrayX  is Stream_Element_Array (1 .. This'Length);
      function To_Str is new Ada.Unchecked_Conversion (ArrayX, StringX);
   begin
      return To_Str (This);
   end To_string;

end Agpl.Streams;
