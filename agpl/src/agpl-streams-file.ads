 

--  Wraps around a regular Stream_IO stream to provide Available_Read/Write
--  functions.

with Agpl.Streams.Observable;

with Ada.Streams.Stream_IO;
use  Ada;

package Agpl.Streams.File is

   --  pragma Elaborate_body

   ------------------------------------------------------------------------
   -- Stream_type                                                        --
   ------------------------------------------------------------------------
   type Stream_type is new Agpl.Streams.Observable.Stream_Type with private;
   type Stream_access is access all Stream_type;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   --  The Index function shouldn't be called after this creation!!
   procedure Create (
      Stream :    out Stream_Type;
      From   : in     Ada.Streams.Stream_IO.File_Type);

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
   --  Doesn't check free space, always return the largest possible value.
   function Available_write (Stream : in Stream_type)
      return Stream_element_count;
   function Available_write (Stream : in Stream_type)
      return Natural;
   pragma Inline (Available_write);

private

   type Stream_type is new Agpl.Streams.Observable.Stream_Type
   with record
      Back           : Agpl.Streams.Stream_Access; -- To read/write
      Available_Read : Stream_element_count;       -- Pending for read data.
   end record;

end Agpl.Streams.File;
