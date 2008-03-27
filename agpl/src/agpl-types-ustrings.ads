 

with Agpl.Dynamic_Vector;

with Ada.Streams;
with Ada.Strings.Unbounded;

package Agpl.Types.Ustrings is

   pragma Preelaborate;

   ------------------------------------------------------------------------
   -- Unbounded String Facilities                                        --
   ------------------------------------------------------------------------
   package ASU renames Ada.Strings.Unbounded;

   subtype UString is ASU.Unbounded_String;

   type Ustring_Array is array (Positive range <>) of Ustring;

   function To_String (U : UString) return String  renames ASU.To_string;

   function To_Ustring (S : String) return UString renames ASU.To_unbounded_string;

   function S (U : UString)         return String  renames ASU.To_string;

   function U (S : String)          return UString renames ASU.To_unbounded_string;

   function "+" (U : UString)       return String  renames ASU.To_string;

   function "+" (S : String)        return UString renames ASU.To_unbounded_string;

   Null_Ustring : UString renames ASU.Null_Unbounded_String;
   Nul          : UString renames ASU.Null_Unbounded_String;

   ------------------------------------------------------------------------
   -- Write_To_Stream                                                    --
   ------------------------------------------------------------------------
   --  Writes the Ustring contents to a stream
   procedure Write_To_Stream (
      This : in Ustring; Stream : access Ada.Streams.Root_Stream_Type'Class);

   ------------------------------------------------------------------------
   -- Package for dynamic arrays                                         --
   ------------------------------------------------------------------------
   package Ustring_Vector is new Agpl.Dynamic_Vector (Ustring);

end Agpl.Types.Ustrings;
