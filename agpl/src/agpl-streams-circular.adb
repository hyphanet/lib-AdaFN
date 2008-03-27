 

--  Circular stream. This is a buffering stream where the written data
--    can be read afterwards in typical producer/consumer fashion.

with Ada.Unchecked_deallocation;
--  with Text_Io;

package body Agpl.Streams.Circular is

   procedure Free is new Ada.Unchecked_deallocation
     (Buffer_type, Buffer_type_access);

   ------------------------------------------------------------------------
   -- Overriden primitives                                               --
   ------------------------------------------------------------------------
   procedure Read(
      Stream : in out Stream_type;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      To_read : constant Stream_element_count := Stream_element_count'Min (
         Item'Length, Available_read (Stream));
      --  Aux is the amount of data read before the split (see lower):
      Aux     : Stream_element_count;
   begin
      --  Check nothing written:
      if Stream.Buffer.Data = null then
         Last := Item'First - 1;
         return;
      end if;

      --  No circular split:
      if Stream.Buffer.Data'Last - Stream.Pos_read + 1 >= To_read then
         Item (Item'First .. Item'First + To_read - 1) :=
         Stream.Buffer.Data (Stream.Pos_read .. Stream.Pos_read + To_read -1);
         Stream.Pos_read := Stream.Pos_read + To_read;
      else
         --  Circular splitting:
         Aux := Stream.Buffer.Data'Last - Stream.Pos_read + 1;
         Item (Item'First .. Item'First + Aux - 1) :=
            Stream.Buffer.Data (Stream.Pos_read .. Stream.Buffer.Data'Last);
         Item (Item'First + Aux .. Item'First + Aux + (To_read - Aux) - 1) :=
            Stream.Buffer.Data (Stream.Buffer.Data'First ..
               Stream.Buffer.Data'First + (To_read - Aux) - 1);
         Stream.Pos_read := Stream.Buffer.Data'First + To_read - Aux;
      end if;
      if Stream.Pos_read > Stream.Buffer.Data'Last then
         Stream.Pos_read := Stream.Buffer.Data'First;
      end if;
      --  Text_Io.Put_Line ("Circular.Read:" & To_read'img);
      Stream.Available_read := Stream.available_read - To_read;
      Last := Item'First + To_read - 1;
   end Read;

   procedure Write(
      Stream : in out Stream_type;
      Item   : in Stream_Element_Array)
   is
      --  Aux is the amount written before the split:
      Aux : Stream_element_count;
   begin
      --  Text_io.Put_Line ("Circular.Write:" & Integer'Image (Item'Length));
      --  Enough room?
      if Available_write (Stream) < Natural'(Item'Length) then
         --  Text_Io.Put_Line ("Av.write: " & Natural'Image (Available_Write (Stream)));
         --  Text_Io.Put_Line ("It.lengt: " & Natural'Image (Item'Length));
         raise Constraint_Error;
      end if;
      --  Allocation on first use:
      if Stream.Buffer.Data = null then
         --  Text_Io.Put_Line ("Buffer Size:" & Stream.Buffer.Size'Img);
         Stream.Buffer.Data := new Buffer_type (1 .. Stream.Buffer.Size);
      end if;
      --  No circular split:
      if Item'Length <= Stream.Buffer.Data'Last - Stream.Pos_write + 1 then
         Stream.Buffer.Data (Stream.Pos_write ..
            Stream.Pos_write + Item'Length - 1) := Item;
         Stream.Pos_write := Stream.Pos_write + Item'Length;
      else
         --  Circular split:
         Aux := Stream.Buffer.Data'Last - Stream.Pos_write + 1;
         Stream.Buffer.Data (Stream.Pos_write .. Stream.Buffer.Data'Last) :=
            Item (Item'First .. Item'First + Aux - 1);
         Stream.Buffer.Data (Stream.Buffer.Data'First ..
            Stream.Buffer.Data'First + (Item'Length - Aux) - 1) :=
               Item (Item'First + Aux .. Item'First + Aux +
                  (Item'Length - Aux) - 1);
         Stream.Pos_write := Stream.Buffer.Data'First + Item'Length - Aux;
      end if;
      if Stream.Pos_write > Stream.Buffer.Data'Last then
         Stream.Pos_write := Stream.Buffer.Data'First;
      end if;
      Stream.Available_read := Stream.Available_read + Item'Length;
   end Write;

   ------------------------------------------------------------------------
   -- Available_read                                                     --
   ------------------------------------------------------------------------
   --  Says how many data has been written but not read:
   function Available_read (Stream : in Stream_type)
      return Stream_element_count
   is
   begin
      return Stream.Available_read;
   end Available_read;

   function Available_read (Stream : in Stream_type) return Natural is
   begin
      return Natural (Stream.Available_read);
   end Available_read;

   ------------------------------------------------------------------------
   -- Available_write                                                    --
   ------------------------------------------------------------------------
   --  Says how many data has been written but not read:
   function Available_write (Stream : in Stream_type)
      return Stream_element_count
   is
   begin
      return Stream.Size - Stream.Available_read;
   end Available_write;

   function Available_write (Stream : in Stream_type) return Natural is
   begin
      return Natural (Stream.Size - Stream.Available_read);
   end Available_write;

   ------------------------------------------------------------------------
   -- Reset                                                              --
   ------------------------------------------------------------------------
   --  Resets everything to the starting point
   procedure Reset (Stream : in out Stream_type) is
   begin
      Free (Stream.Buffer.Data);
      Stream.Pos_write := 1;
      Stream.Pos_read  := 1;
      Stream.Available_read := 0;
   end Reset;

   procedure Finalize   (This: in out Controlled_buffer_type) is
   begin
      Free (This.Data);
   end Finalize;

end Agpl.Streams.Circular;
