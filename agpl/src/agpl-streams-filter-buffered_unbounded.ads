 

--  Filter stream who allows to buffer for write or read some data. Just a
--  convenience to use Circular_Unbounded.

with Agpl.Streams.Circular_Unbounded;
with Agpl.Streams.Filter;

package Agpl.Streams.Filter.Buffered_Unbounded is

   pragma Preelaborate;

   --  These parameters affect new instances (a-la Float_Io).
   Max_Buffer_Size : Stream_Element_Count := Stream_Element_Count'Last;
   Initial_Buffer  : Stream_Element_Count := 4096;
   Grow_Percent    : Positive             := 50;

   pragma Atomic (Max_Buffer_Size);
   pragma Atomic (Initial_Buffer);
   pragma Atomic (Grow_Percent);

   ------------------------------------------------------------------------
   -- Stream_type                                                        --
   ------------------------------------------------------------------------
   type Stream_type is new Filter.Stream_Type with private;

   type Stream_access is access all Stream_type'Class;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   --  Max_Memory_Usage can cause an internal growing to fail.
   --  Grow_Factor stablishes how many unused space will be allocated upon
   --  buffer expansion. (100 causes as many used as free to be allocated)
   --  If Lazy, buffers allocation is not done until first writing.
   procedure Create (
      This              : in out Stream_Type;
      Back              : access Ada.Streams.Root_Stream_Type'Class;
      Max_Memory_Usage  : in     Stream_Element_Count := 1024 * 1024;
      Initial_Size      : in     Stream_Element_Count := 1024 * 4;
      Grow_Factor       : in     Natural              := 100;
      Lazy              : in     Boolean              := True);

   ------------------------------------------------------------------------
   -- Read                                                               --
   ------------------------------------------------------------------------
   --  Will read from internal buffer
   procedure Read (
      This : in out Stream_Type;
      Item :    out Ada.Streams.Stream_Element_Array;
      Last :    out Ada.Streams.Stream_Element_Offset);

   ------------------------------------------------------------------------
   -- Write                                                              --
   ------------------------------------------------------------------------
   --  Will cache all data written
   procedure Write (
      This : in out Stream_Type;
      Item : in     Ada.Streams.Stream_Element_Array);

   ------------------------------------------------------------------------
   -- Reset                                                              --
   ------------------------------------------------------------------------
   --  Cleans everything inside
   procedure Reset (This : in out Stream_Type);

   ------------------------------------------------------------------------
   -- Flush_Read                                                         --
   ------------------------------------------------------------------------
   --  Tries to read so many data from the back stream.
   procedure Flush_Read (
      This  : in out Stream_type;
      Count : in     Stream_Element_Count := Stream_Element_Count'Last);

   ------------------------------------------------------------------------
   -- Prefetch                                                           --
   ------------------------------------------------------------------------
   --  A "pull" procedure to get data into de filter from the "read" end,
   --  but without needing the data to be actually read from the filter.
   --  Equal to Flush_Read
   procedure Prefetch (
      This  : in out Stream_Type;
      Count : in     Stream_Element_Count) renames Flush_Read;

   ------------------------------------------------------------------------
   -- Flush_Write                                                        --
   ------------------------------------------------------------------------
   --  Writes to the back buffer as many data as indicated or less if not as
   --  many is cached.
   procedure Flush_Write (
      This  : in out Stream_type;
      Count : in     Stream_Element_Count := Stream_Element_Count'Last);

   ------------------------------------------------------------------------
   -- Available_Read                                                     --
   ------------------------------------------------------------------------
   --  Returns the amount of buffered data available to be read.
   function Available_Read (This : in Stream_Type)
      return Stream_Element_Count;

   ------------------------------------------------------------------------
   -- Available_Write                                                    --
   ------------------------------------------------------------------------
   --  Returns Max_Memory_Usage
   function Available_Write (This : in Stream_Type)
      return Stream_Element_Count;

   ------------------------------------------------------------------------
   -- Get_Buffered_Write_Count                                           --
   ------------------------------------------------------------------------
   --  Returns the amount of written buffered data.
   function Get_Buffered_Write_Count (This : in Stream_Type)
      return Stream_Element_Count;
   pragma Inline (Get_Buffered_Write_Count);

private

   package Circular renames Agpl.Streams.Circular_Unbounded;

   type Stream_type is new Filter.Stream_Type with
   record
      Buf_In   : Circular.Stream_Type;
      Buf_Out  : Circular.Stream_Type;
   end record;

end Agpl.Streams.Filter.Buffered_Unbounded;
