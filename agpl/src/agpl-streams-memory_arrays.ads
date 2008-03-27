 

--  Allows reading from an array viewed as an stream:
--  or writing to it.
--  Mixing the two operations is erroneous.

with Ada.Streams;                use Ada.Streams;

package Agpl.Streams.Memory_arrays is

   pragma Preelaborate;

   --  New stream type:
   --  Takes an access to an element array. It should remain allocated
   --  during the life of this stream.
   --  It will not be deallocated
   type Stream_type (
      Data  : access Stream_element_array) is new
      Ada.Streams.Root_Stream_Type with private;

   type Stream_access is access all Stream_type;

   ------------------------------------------------------------------------
   -- Read                                                               --
   ------------------------------------------------------------------------
   procedure Read (Stream : in out Stream_Type;
                   Item   :    out Stream_Element_Array;
                   Last   :    out Stream_Element_Offset);
   pragma Inline (Read);

   ------------------------------------------------------------------------
   -- Write                                                              --
   ------------------------------------------------------------------------
   --  Can cause constraint_error if the supplied buffer is exhausted.
   procedure Write (Stream : in out Stream_Type;
                    Item   : in     Stream_Element_Array);
   pragma Inline (Write);

   ------------------------------------------------------------------------
   -- Index                                                              --
   ------------------------------------------------------------------------
   --  Amount of data written / read.
   function Index (Stream : in Stream_type) return Stream_element_offset;

   ------------------------------------------------------------------------
   -- Set_index                                                          --
   ------------------------------------------------------------------------
   --  Set starting position for read/write (First is 1)
   procedure Set_index (
      Stream : in out Stream_type;
      Index  : in     Stream_element_offset);

   ------------------------------------------------------------------------
   -- End_of_stream                                                      --
   ------------------------------------------------------------------------
   function End_of_stream (Stream : in Stream_type) return Boolean;

private

   type Stream_type (
      Data  : access Stream_element_array) is new Ada.Streams.Root_Stream_Type
   with record
      Pos : Stream_element_offset := Data.all'First;
   end record;

end Agpl.Streams.Memory_arrays;
