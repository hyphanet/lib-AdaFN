 

--  Allows reading from a string viewed as an stream:

package body Agpl.Streams.Memory_arrays is

   ------------------------------------------------------------------------
   -- Read                                                               --
   ------------------------------------------------------------------------
   procedure Read (
      Stream : in out Stream_type;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset) is
      Pos    : Stream_element_offset renames Stream.Pos;
   begin
      if Pos > Stream.Data'Last then
         raise Constraint_Error;
      end if;
      if Item'Length > Stream.Data'Last - Pos + 1 then
         Item (Item'First .. Item'First + Stream.Data'Last - Pos) :=
            Stream.Data (Pos .. Stream.Data'Last);
         Last := Item'First + Stream.Data'Last - Pos;
         Pos  := Stream.Data'Last + 1;
      else
         Item := Stream.Data (Pos .. Pos + Item'Length - 1);
         Last := Item'Last;
         Pos  := Pos + Item'Length;
      end if;
   end Read;

   ------------------------------------------------------------------------
   -- Write                                                              --
   ------------------------------------------------------------------------
   procedure Write (
      Stream : in out Stream_type;
      Item   : in Stream_Element_Array) is
      Pos    : Stream_element_offset renames Stream.Pos;
   begin
      if Pos + Item'Length - 1 > Stream.Data'Length then
         raise Constraint_Error;
      end if;
      Stream.Data (Pos .. Pos + Item'Length - 1) := Item;
      Pos := Pos + Item'Length;
   end Write;

   ------------------------------------------------------------------------
   -- Index                                                              --
   ------------------------------------------------------------------------
   function Index (Stream : in Stream_type) return Stream_element_offset is
   begin
      return Stream.Pos - Stream.Data'First;
   end Index;

   ------------------------------------------------------------------------
   -- Set_index                                                          --
   ------------------------------------------------------------------------
   --  Set starting position for read/write (First is 1)
   procedure Set_index (
      Stream : in out Stream_type;
      Index  : in     Stream_element_offset) is
   begin
      Stream.Pos := Stream.Data'First + Index - 1;
   end Set_index;

   ------------------------------------------------------------------------
   -- End_of_stream                                                      --
   ------------------------------------------------------------------------
   function End_of_stream (Stream : in Stream_type) return Boolean is
   begin
      return Stream.Pos > Stream.Data.all'Last;
   end End_of_stream;

end Agpl.Streams.Memory_arrays;
