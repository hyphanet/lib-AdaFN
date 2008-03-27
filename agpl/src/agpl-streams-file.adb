 

--  Wraps around a regular Stream_IO stream to provide Available_Read/Write
--  functions

--  with Text_Io;

package body Agpl.Streams.File is

   package Stream_IO renames Ada.Streams.Stream_IO;
   use type Stream_IO.Count;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   --  The Index function shouldn't be called after this creation!!
   procedure Create
     (Stream : out Stream_type;
      From   : in Ada.Streams.Stream_IO.File_Type)
   is
   begin
      Stream.Back           :=
         Agpl.Streams.Stream_access (Stream_IO.Stream (From));
      Stream.Available_Read :=
         Stream_Element_Offset (Stream_IO.Size (From) -
                                Stream_IO.Index (From) +
                                1);
   end Create;

   ------------------------------------------------------------------------
   -- Overriden primitives                                               --
   ------------------------------------------------------------------------
   procedure Read
     (Stream : in out Stream_type;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
   begin
      --  Text_Io.Put_Line ("File remaining:" & Stream.Available_Read'Img);
      --  Text_Io.Put_Line ("Attempting read of:" & Item'Length'Img);
      Ada.Streams.Read (Stream.Back.all, Item, Last);
      Stream.Available_Read := Stream.Available_Read -
                               (Last - Item'First + 1);
   end Read;

   procedure Write
     (Stream : in out Stream_type;
      Item   : in Stream_Element_Array)
   is
   begin
      Ada.Streams.Write (Stream.Back.all, Item);
   end Write;

   ------------------------------------------------------------------------
   -- Available_read                                                     --
   ------------------------------------------------------------------------
   --  Says how many data has been written but not read:
   function Available_read
     (Stream : in Stream_type)
      return   Stream_Element_Count
   is
   begin
      return Stream.Available_Read;
   end Available_read;

   function Available_read (Stream : in Stream_type) return Natural is
   begin
      return Natural (Stream_Element_Count'(Available_read (Stream)));
   end Available_read;

   ------------------------------------------------------------------------
   -- Available_write                                                    --
   ------------------------------------------------------------------------
   function Available_Write
     (Stream : in Stream_type)
      return   Stream_Element_Count
   is
      pragma Unreferenced (Stream);
   begin
      return Stream_Element_Count'Last;
   end Available_Write;

   function Available_Write (Stream : in Stream_type) return Natural is
      pragma Unreferenced (Stream);
   begin
      return Natural'Last;
   end Available_Write;

end Agpl.Streams.File;
