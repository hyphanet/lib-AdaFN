 

--  Abstract class; A filter stream takes an access to a back stream from
--  which it can read/write, performing some extra operation/filtering if
--  needed.

with Agpl.Exceptions;

package body Agpl.Streams.Filter is

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   --  The default creation procedure assigns the back filter to the stream.
   procedure Create (
      This : in out Stream_Type;
      Back : access Ada.Streams.Root_Stream_Type'Class) is
   begin
      This.Back := Agpl.Streams.Stream_Access (Back);
   end Create;

   ------------------------------------------------------------------------
   -- Read                                                               --
   ------------------------------------------------------------------------
   procedure Read (
      This : in out Stream_Type;
      Item :    out Ada.Streams.Stream_Element_Array;
      Last :    out Ada.Streams.Stream_Element_Offset)
   is
   begin
      Ada.Streams.Read (This.Back.all, Item, Last);
   end Read;

   ------------------------------------------------------------------------
   -- Write                                                              --
   ------------------------------------------------------------------------
   procedure Write (
      This : in out Stream_Type;
      Item : in     Ada.Streams.Stream_Element_Array) is
   begin
      Ada.Streams.Write (This.Back.all, Item);
   end Write;

   ------------------------------------------------------------------------
   -- Prefetch                                                           --
   ------------------------------------------------------------------------
   --  A "pull" procedure to get data into de filter from the "read" end,
   --  but without needing the data to be actually read from the filter.
   --  Unimplemented for the default filter. (It hasn't caching capabilities).
   procedure Prefetch (
      This  : in out Stream_Type;
      Count : in     Stream_Element_Count)
   is
      pragma Unreferenced (This);
      pragma Unreferenced (Count);
   begin
      raise Exceptions.Unimplemented;
   end Prefetch;

   ------------------------------------------------------------------------
   -- Available_Read                                                     --
   ------------------------------------------------------------------------
   --  Unlimited
   function Available_Read (This : in Stream_Type)
      return Stream_Element_Count
   is
   pragma Unreferenced (This);
   begin
      return Stream_Element_Count'Last;
   end Available_Read;

   ------------------------------------------------------------------------
   -- Available_Write                                                    --
   ------------------------------------------------------------------------
   --  Unlimited
   function Available_Write (This: in Stream_Type)
      return Stream_Element_Count is
      pragma Unreferenced (This);
   begin
      return Stream_Element_Count'Last;
   end Available_Write;

end Agpl.Streams.Filter;
