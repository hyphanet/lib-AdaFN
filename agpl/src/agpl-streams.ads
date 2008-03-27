 

with Ada.Streams;
with Ada.Unchecked_Deallocation;

package Agpl.Streams is

   pragma Preelaborate;

   ------------------------------------------------------------------------
   -- Types                                                              --
   ------------------------------------------------------------------------

   type Stream_access is access all Ada.Streams.Root_stream_type'Class;

   type Stream_element_array_access is access all
      Ada.Streams.Stream_element_array;

   ------------------------------------------------------------------------
   -- Utilities                                                          --
   ------------------------------------------------------------------------

   ----------
   -- Free --
   ----------
   procedure Free is new Ada.Unchecked_Deallocation (
      Ada.Streams.Stream_Element_Array, Stream_Element_Array_Access);
   procedure Free is new Ada.Unchecked_Deallocation (
      Ada.Streams.Root_Stream_Type'Class, Stream_Access);

   ---------------
   -- To_string --
   ---------------
   --  Returns a string having the characters in the stream array.
   --  Uses unchecked conversion
   function To_string (This : in Ada.Streams.Stream_element_array)
                       return    String;

   -----------------------------
   -- To_Stream_Element_Array --
   -----------------------------
   --  Uses unchecked conversion
   function To_Stream_Element_Array (This : in String)
                                     return    Ada.Streams.Stream_Element_Array;

   --  Facilities for children packages.

   subtype Root_Stream_Type      is Ada.Streams.Root_Stream_Type;

   subtype Stream_Element_Array  is Ada.Streams.Stream_Element_Array;
   subtype Stream_Element_Count  is Ada.Streams.Stream_Element_Count;
   subtype Stream_Element_Offset is Ada.Streams.Stream_Element_Offset;
   use type Stream_Element_Array;
   use type Stream_Element_Count;
   use type Stream_Element_Offset;

end Agpl.Streams;
