 

--  Circular stream. This is a buffering stream where the written data
--    can be read afterwards in typical producer/consumer fashion.

with Agpl.Streams.Observable;

with Ada.Finalization;

package Agpl.Streams.Circular is

   pragma Preelaborate;

   ------------------------------------------------------------------------
   -- Stream_type                                                        --
   ------------------------------------------------------------------------
   --  The size of the intermediate buffer is the maximum non-read data we
   --  can have:
   type Stream_type (Size : Stream_element_count) is new
      Streams.Observable.Stream_Type with private;
   type Stream_access is access all Stream_type;

   ------------------------------------------------------------------------
   -- Overriden primitives                                               --
   ------------------------------------------------------------------------
   procedure Read(
      Stream : in out Stream_type;
      Item   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset);

   procedure Write(
      Stream : in out Stream_type;
      Item   : in     Stream_Element_Array);

   ------------------------------------------------------------------------
   -- Available_read                                                     --
   ------------------------------------------------------------------------
   --  Says how many data has been written but not read:
   function Available_read (Stream : in Stream_type)
      return Stream_element_count;
   function Available_read (Stream : in Stream_type)
      return Natural;
   pragma Inline (Available_read);

   ------------------------------------------------------------------------
   -- Available_write                                                    --
   ------------------------------------------------------------------------
   --  Says how many data can be written to the stream:
   function Available_write (Stream : in Stream_type)
      return Stream_element_count;
   function Available_write (Stream : in Stream_type)
      return Natural;
   pragma Inline (Available_write);

   ------------------------------------------------------------------------
   -- Reset                                                              --
   ------------------------------------------------------------------------
   --  Resets everything to the starting point
   procedure Reset (Stream : in out Stream_type);

private

   subtype Buffer_type is
      Stream_element_array;
   type Buffer_type_access is access all Buffer_type;

   --  Allocation is made in the first use to delay memory consumption:
   type Controlled_buffer_type (Size : Stream_element_count) is new
   Ada.Finalization.Controlled with
      record
         Data: Buffer_type_access;
      end record;

   procedure Finalize   (this: in out Controlled_buffer_type);

   type Stream_type (Size : Stream_element_count) is new
      Streams.Observable.Stream_Type
   with record
      Buffer    : Controlled_buffer_type (Size);
      Pos_read  : Stream_element_offset := 1; -- Next element to read.
      Pos_Write : Stream_element_offset := 1; -- Next element to write.
      Available_read : Stream_element_count  := 0; -- Pending for read data.
   end record;

end Agpl.Streams.Circular;
