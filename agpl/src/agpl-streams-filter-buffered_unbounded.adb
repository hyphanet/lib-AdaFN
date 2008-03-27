 

--  Filter stream who allows to buffer for write or read some data. Just a
--  convenience to use Circular_Unbounded.

package body Agpl.Streams.Filter.Buffered_Unbounded is

   Chunk_Size : constant := 4096;

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
      Lazy              : in     Boolean              := True)
   is
   begin
      This.Back := Streams.Stream_Access (Back);

      Circular.Create (
         This.Buf_In,
         Max_Memory_Usage,
         Initial_Size,
         Grow_Factor,
         Lazy);
      Circular.Create (
         This.Buf_Out,
         Max_Memory_Usage,
         Initial_Size,
         Grow_Factor,
         Lazy);
   end Create;

   ------------------------------------------------------------------------
   -- Read                                                               --
   ------------------------------------------------------------------------
   --  Will read from internal buffer
   procedure Read (
      This : in out Stream_Type;
      Item :    out Ada.Streams.Stream_Element_Array;
      Last :    out Ada.Streams.Stream_Element_Offset) is
   begin
      Circular.Read (This.Buf_In, Item, Last);
   end Read;

   ------------------------------------------------------------------------
   -- Write                                                              --
   ------------------------------------------------------------------------
   --  Will cache all data written
   procedure Write (
      This : in out Stream_Type;
      Item : in     Ada.Streams.Stream_Element_Array) is
   begin
      Circular.Write (This.Buf_Out, Item);
   end Write;

   ------------------------------------------------------------------------
   -- Reset                                                              --
   ------------------------------------------------------------------------
   --  Cleans everything inside
   procedure Reset (This : in out Stream_Type) is
   begin
      Circular.Reset (This.Buf_In);
      Circular.Reset (This.Buf_Out);
   end Reset;

   ------------------------------------------------------------------------
   -- Flush_Read                                                         --
   ------------------------------------------------------------------------
   --  Tries to read so many data from the back stream.
   procedure Flush_Read (
      This  : in out Stream_type;
      Count : in     Stream_Element_Count := Stream_Element_Count'Last)
   is
      Last : Stream_Element_Offset;
      Rest : Stream_Element_Count := Count;
   begin
      loop
         declare
            Item : Stream_Element_Array (1 .. Stream_Element_Count'Min (Rest, Chunk_Size));
         begin
            Ada.Streams.Read (This.Back.all, Item, Last);
            exit when Last < Item'First;

            Circular.Write (This.Buf_In, Item (1 .. Last));
            Rest := Rest - Last;
            exit when Rest = 0;
         end;
      end loop;
   end Flush_Read;

   ------------------------------------------------------------------------
   -- Flush_Write                                                        --
   ------------------------------------------------------------------------
   --  Writes to the back buffer as many data as indicated or less if not as many is cached.
   procedure Flush_Write (
      This  : in out Stream_type;
      Count : in     Stream_Element_Count := Stream_Element_Count'Last)
   is
      Last : Stream_Element_Offset;
      Rest : Stream_Element_Count := Count;
   begin
      loop
         declare
            Item : Stream_Element_Array (1 .. Stream_Element_Count'Min (Rest, Chunk_Size));
         begin
            Circular.Read (This.Buf_Out, Item, Last);
            exit when Last < Item'First;

            Ada.Streams.Write (This.Back.all, Item (1 .. Last));
            Rest := Rest - Last;
            exit when Rest = 0;
         end;
      end loop;
   end Flush_Write;

   ------------------------------------------------------------------------
   -- Available_Read                                                     --
   ------------------------------------------------------------------------
   --  Returns the amount of read buffered data.
   function Available_Read (This : in Stream_Type)
      return Stream_Element_Count is
   begin
      return Circular.Available_Read (This.Buf_In);
   end Available_Read;

   ------------------------------------------------------------------------
   -- Available_Write                                                    --
   ------------------------------------------------------------------------
   --  Returns Max_Memory_Usage
   function Available_Write (This : in Stream_Type)
      return Stream_Element_Count is
   begin
      return Circular.Available_Write (This.Buf_Out);
   end Available_Write;

   ------------------------------------------------------------------------
   -- Get_Buffered_Write_Count                                           --
   ------------------------------------------------------------------------
   --  Returns the amount of written buffered data.
   function Get_Buffered_Write_Count (This : in Stream_Type)
      return Stream_Element_Count is
   begin
      return Circular.Available_Read (This.Buf_Out);
   end Get_Buffered_Write_Count;

end Agpl.Streams.Filter.Buffered_Unbounded;
