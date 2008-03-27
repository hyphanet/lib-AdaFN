 

--  Circular stream. This is a buffering stream where the written data
--    can be read afterwards in typical producer/consumer fashion.
--  The internal buffer will grow as needed to acommodate unread data.
--  The internal buffer isn't allocated until the first writting to save memory
--  usage.
--  Additionally, it is controlled.

with Agpl.Streams.Observable;

package Agpl.Streams.Circular_Unbounded is

   pragma Preelaborate;

   Memory_Limit_Reached : exception;

   ------------------------------------------------------------------------
   -- Stream_type                                                        --
   ------------------------------------------------------------------------
   type Stream_type
   is new Agpl.Streams.Observable.Stream_Type with private;

   type Stream_access is access all Stream_type'Class;

   ------------------------------------------------------------------------
   -- Create                                                            --
   ------------------------------------------------------------------------
   --  Max_Memory_Usage can cause an internal growing to fail.
   --  Grow_Factor stablishes how many unused space will be allocated upon
   --  buffer expansion. (100 causes as many used as free to be allocated)
   --  If Lazy, buffers allocation is not done until first writing.
   procedure Create (
      This              : in out Stream_Type;
      Max_Memory_Usage  : in     Stream_Element_Count := 1024 * 1024;
      Initial_Size      : in     Stream_Element_Count := 1024 * 4;
      Grow_Factor       : in     Natural              := 100;
      Lazy              : in     Boolean              := True);

   ------------------------------------------------------------------------
   -- Overriden primitives                                               --
   ------------------------------------------------------------------------
   procedure Read(
      This : in out Stream_type;
      Item :    out Stream_Element_Array;
      Last :    out Stream_Element_Offset);

   procedure Write(
      This : in out Stream_type;
      Item : in     Stream_Element_Array);

   ------------------------------------------------------------------------
   -- Peek                                                               --
   ------------------------------------------------------------------------
   --  Returns data but without effectively consuming it
   procedure Peek (
      This : in out Stream_type;
      Item :    out Stream_Element_Array;
      Last :    out Stream_Element_Offset);

   ------------------------------------------------------------------------
   -- Skip                                                               --
   ------------------------------------------------------------------------
   --  Skip these readable data
   procedure Skip (
      This  : in out Stream_Type;
      Count : in     Stream_Element_Count);

   ------------------------------------------------------------------------
   -- Available_read                                                     --
   ------------------------------------------------------------------------
   --  Says how many data has been written but not read:
   function Available_read (Stream : in Stream_type)
      return Stream_element_count;
   pragma Inline (Available_read);
   function Available_read (Stream : in Stream_type)
      return Natural;
   pragma Inline (Available_read);

   ------------------------------------------------------------------------
   -- Available_write                                                    --
   ------------------------------------------------------------------------
   --  Says how many data can be written to the stream:
   --  Only limited by the max_memory_usage parameter on creation.
   function Available_write (Stream : in Stream_type)
      return Stream_element_count;
   function Available_write (Stream : in Stream_type)
      return Natural;
   pragma Inline (Available_write);

   ------------------------------------------------------------------------
   -- Reset                                                              --
   ------------------------------------------------------------------------
   --  Resets everything to the starting point (frees memory too)
   procedure Reset (Stream : in out Stream_type);

private

   procedure Finalize (This : in out Stream_Type);

   type Stream_type
   is new Agpl.Streams.Observable.Stream_Type with record
      Buffer    : Stream_Element_Array_Access;
      Pos_read  : Stream_Element_Offset       := 1; -- Next element to read.
      Pos_Write : Stream_Element_Offset       := 1; -- Next element to write.
      Available_read  : Stream_Element_Count  := 0; -- Data pending to be read.
      Available_write : Stream_Element_Count  := 0; -- Free space.

      Initial_Size    : Stream_Element_Count  := 0;
      Max_Memory_Usage: Stream_Element_Count  := 0;
      Grow_Factor     : Natural               := 0;
      Lazy            : Boolean               := True;
   end record;

end Agpl.Streams.Circular_Unbounded;
