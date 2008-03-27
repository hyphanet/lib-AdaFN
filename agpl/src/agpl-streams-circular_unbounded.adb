 

--  Circular stream. This is a buffering stream where the written data
--    can be read afterwards in typical producer/consumer fashion.
--  The internal buffer will grow as needed to acommodate unread data.

package body Agpl.Streams.Circular_Unbounded is

   ------------------------------------------------------------------------
   -- Allocate_Buffer                                                    --
   ------------------------------------------------------------------------
   procedure Allocate_Buffer (This : in out Stream_Type);
   pragma Inline (Allocate_Buffer);

   procedure Allocate_Buffer (This : in out Stream_Type) is
   begin
      if This.Buffer = null then
         This.Buffer := new Stream_Element_Array (1 .. This.Initial_Size);
      end if;
   end Allocate_Buffer;

   ------------------------------------------------------------------------
   -- Create                                                            --
   ------------------------------------------------------------------------
   procedure Create (
      This              : in out Stream_Type;
      Max_Memory_Usage  : in     Stream_Element_Count := 1024 * 1024;
      Initial_Size      : in     Stream_Element_Count := 1024 * 4;
      Grow_Factor       : in     Natural              := 100;
      Lazy              : in     Boolean              := True)
   is
   begin
      This.Available_Write  := Initial_Size;
      This.Initial_Size     := Initial_Size;
      This.Max_Memory_Usage := Max_Memory_Usage;
      This.Grow_Factor      := Grow_Factor;
      This.Lazy             := Lazy;
      Reset (This);
   end Create;

   ------------------------------------------------------------------------
   -- Read                                                               --
   ------------------------------------------------------------------------
   procedure Read (
      This : in out Stream_type;
      Item :    out Stream_Element_Array;
      Last :    out Stream_Element_Offset)
   is
      To_Read : constant Stream_Element_Count := Stream_Element_Count'Min (
         This.Available_Read, Item'Length);
   begin
      if This.Available_Read = 0 then
         Last := Item'First - 1;
         return;
      end if;

      declare
         Chunk_Length : constant Stream_Element_Count := Stream_Element_Count'Min (
            To_Read, This.Buffer'Last - This.Pos_Read + 1);
         Remaining    : constant Stream_Element_Count := To_Read - Chunk_Length;
      begin
         Item (Item'First .. Item'First + Chunk_Length - 1) :=
            This.Buffer (This.Pos_Read .. This.Pos_Read + Chunk_Length - 1);
         if Remaining > 0 then
            Item (Item'First + Chunk_Length .. Item'First + To_Read - 1) :=
               This.Buffer (This.Buffer'First .. This.Buffer'First + Remaining - 1);
            This.Pos_Read := This.Buffer'First + Remaining;
         else
            This.Pos_Read := This.Pos_Read + To_Read;
         end if;
      end;

      This.Available_Read  := This.Available_Read  - To_Read;
      This.Available_Write := This.Available_Write + To_Read;
      Last                 := Item'First + To_Read - 1;

      pragma Assert (This.Available_Read + This.Available_Write = This.Buffer'Length);
   end Read;

   ------------------------------------------------------------------------
   -- Realloc                                                            --
   ------------------------------------------------------------------------
   --  Reallocates a growth buffer
   --  Size = Used * (1 + Grow_Factor);
   procedure Realloc (
      This : in out Stream_Type;
      Used : in     Stream_Element_Count)
   is
      New_Size : constant Stream_Element_Count :=
         Stream_Element_Count (Float (Used) * Float (100 + This.Grow_Factor) / 100.0);
      New_Data : Stream_Element_Array_Access;
      Last     : Stream_Element_Offset;
      Av_Read  : constant Stream_Element_Count := This.Available_Read;
   begin
      if New_Size > This.Max_Memory_Usage then
         raise Memory_Limit_Reached;
      end if;

      New_Data := new Stream_Element_Array (1 .. New_Size);

      --  Read the data remaining
      Read (This, New_Data.all, Last);

      --  All data should've been read:
      pragma Assert (Last = New_Data'First + Av_Read - 1);

      --  Adjust stream:
      Free (This.Buffer);
      This.Buffer          := New_Data;
      This.Available_Read  := Av_Read;
      This.Available_Write := This.Buffer'Length - Av_Read;
      This.Pos_Read        := This.Buffer'First;
      This.Pos_Write       := This.Buffer'First  + Av_Read;
      pragma Assert (This.Available_Read + This.Available_Write = This.Buffer'Length);
   end Realloc;

   ------------------------------------------------------------------------
   -- Write                                                              --
   ------------------------------------------------------------------------
   procedure Write (
      This : in out Stream_type;
      Item : in     Stream_Element_Array)
   is
   begin
      Allocate_Buffer (This);

      --  Allocate more memory if needed.
      if This.Available_Write < Stream_Element_Count'(Item'Length) then
         Realloc (This, This.Available_Read + Item'Length);
      end if;

      --  Regular writing:
      declare
         Chunk_Length : constant Stream_Element_Count := Stream_Element_Count'Min (
            Item'Length, This.Buffer'Last - This.Pos_Write + 1);
         Remaining    : constant Stream_Element_Count := Item'Length - Chunk_Length;
      begin
         This.Buffer (This.Pos_Write .. This.Pos_Write + Chunk_Length - 1) :=
            Item (Item'First .. Item'First + Chunk_Length - 1);
         --  Write remaining data if necessary
         if Remaining > 0 then
            This.Buffer (This.Buffer'First .. This.Buffer'First + Remaining - 1) :=
               Item (Item'First + Chunk_Length .. Item'Last);
            This.Pos_Write := This.Buffer'First + Remaining;
         else
            This.Pos_Write := This.Pos_Write + Item'Length;
         end if;
      end;

      This.Available_Read  := This.Available_Read  + Item'Length;
      This.Available_Write := This.Available_Write - Item'Length;

      pragma Assert (This.Available_Read + This.Available_Write = This.Buffer'Length);
   end Write;

   ------------------------------------------------------------------------
   -- Peek                                                               --
   ------------------------------------------------------------------------
   --  Returns data but without effectively consuming it
   procedure Peek (
      This : in out Stream_type;
      Item :    out Stream_Element_Array;
      Last :    out Stream_Element_Offset)
   is
      To_Read : constant Stream_Element_Count := Stream_Element_Count'Min (
         This.Available_Read, Item'Length);
   begin
      if This.Available_Read = 0 then
         Last := Item'First - 1;
         return;
      end if;

      declare
         Chunk_Length : constant Stream_Element_Count := Stream_Element_Count'Min (
            To_Read, This.Buffer'Last - This.Pos_Read + 1);
         Remaining    : constant Stream_Element_Count := To_Read - Chunk_Length;
      begin
         Item (Item'First .. Item'First + Chunk_Length - 1) :=
            This.Buffer (This.Pos_Read .. This.Pos_Read + Chunk_Length - 1);
         if Remaining > 0 then
            Item (Item'First + Chunk_Length .. Item'First + To_Read - 1) :=
               This.Buffer (This.Buffer'First .. This.Buffer'First + Remaining - 1);
         end if;
      end;

      Last := Item'First + To_Read - 1;

      pragma Assert (This.Available_Read + This.Available_Write = This.Buffer'Length);
   end Peek;

   ------------------------------------------------------------------------
   -- Skip                                                               --
   ------------------------------------------------------------------------
   --  Skip these readable data
   procedure Skip (
      This  : in out Stream_Type;
      Count : in     Stream_Element_Count) is
   begin
      if This.Available_Read < Count then
         raise Constraint_Error;
      end if;

      if This.Pos_Read + Count <= This.Buffer'Last then
         This.Pos_Read := This.Pos_Read + Count;
      else
         This.Pos_Read :=
            This.Buffer'First +
            (Count - (This.Buffer'Last - This.Pos_Read + 1));
      end if;

      This.Available_Read  := This.Available_Read  - Count;
      This.Available_Write := This.Available_Write + Count;

      pragma Assert (This.Available_Read + This.Available_Write = This.Buffer'Length);
   end Skip;

   ------------------------------------------------------------------------
   -- Available_read                                                     --
   ------------------------------------------------------------------------
   --  Says how many data has been written but not read:
   function Available_Read (Stream : in Stream_type)
      return Stream_element_count is
   begin
      return Stream.Available_Read;
   end Available_Read;

   function Available_Read (Stream : in Stream_type)
      return Natural is
   begin
      return Natural (Stream.Available_Read);
   end Available_Read;

   ------------------------------------------------------------------------
   -- Available_write                                                    --
   ------------------------------------------------------------------------
   --  Says how many data can be written to the stream:
   --  Only limited by the max_memory_usage parameter on creation.
   function Available_write (Stream : in Stream_type)
      return Stream_element_count is
   begin
      return Stream.Max_Memory_Usage - Stream.Available_Read;
   end Available_Write;

   function Available_write (Stream : in Stream_type)
      return Natural is
   begin
      return Natural (Stream.Max_Memory_Usage - Stream.Available_Read);
   end Available_Write;

   ------------------------------------------------------------------------
   -- Reset                                                              --
   ------------------------------------------------------------------------
   --  Resets everything to the starting point (frees memory too)
   procedure Reset (Stream : in out Stream_type) is
   begin
      Free (Stream.Buffer);
      Stream.Pos_Read        := 1;
      Stream.Pos_Write       := 1;
      Stream.Available_Read  := 0;
      Stream.Available_Write := Stream.Initial_Size;
      if not Stream.Lazy then
         Allocate_Buffer (Stream);
      end if;
   end Reset;

   ------------------------------------------------------------------------
   -- Finalize                                                           --
   ------------------------------------------------------------------------
   procedure Finalize (This : in out Stream_Type) is
   begin
      Free (This.Buffer);
   end Finalize;

end Agpl.Streams.Circular_Unbounded;
