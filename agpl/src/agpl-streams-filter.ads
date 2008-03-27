 

--  Abstract class; A filter stream takes an access to a back stream from
--  which it can read/write, performing some extra operation/filtering if
--  needed.
--  This default implementation does nothing, just passing data along.

with Agpl.Streams.Controlled;

with Ada.Streams;

package Agpl.Streams.Filter is

   pragma Preelaborate;

   ------------------------------------------------------------------------
   -- Exceptions                                                         --
   ------------------------------------------------------------------------
   Unknown_Availability : exception;
   --  Raised when we don't know in advance how many data is available.

   ------------------------------------------------------------------------
   -- Stream_type                                                        --
   ------------------------------------------------------------------------
   --  Since it is derived from Controlled, Initialize and Finalize can be
   --  overriden
   type Stream_Type
   is new Agpl.Streams.Controlled.Stream_Type with private;

   type Stream_Access is access all Stream_type'Class;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   --  The default creation procedure assigns the back filter to the stream.
   procedure Create (
      This : in out Stream_Type;
      Back : access Ada.Streams.Root_Stream_Type'Class);

   ------------------------------------------------------------------------
   -- Read                                                               --
   ------------------------------------------------------------------------
   procedure Read (
      This : in out Stream_Type;
      Item :    out Ada.Streams.Stream_Element_Array;
      Last :    out Ada.Streams.Stream_Element_Offset);

   ------------------------------------------------------------------------
   -- Write                                                              --
   ------------------------------------------------------------------------
   procedure Write (
      This : in out Stream_Type;
      Item : in     Ada.Streams.Stream_Element_Array);

   ------------------------------------------------------------------------
   -- Prefetch                                                           --
   ------------------------------------------------------------------------
   --  A "pull" procedure to get data into de filter from the "read" end,
   --  but without needing the data to be actually read from the filter.
   --  Unimplemented for the default filter. (It hasn't caching capabilities).
   procedure Prefetch (
      This  : in out Stream_Type;
      Count : in     Stream_Element_Count);

   ------------------------------------------------------------------------
   -- Available_Read                                                     --
   ------------------------------------------------------------------------
   --  Unlimited
   function Available_Read (This : in Stream_Type)
      return Stream_Element_Count;

   ------------------------------------------------------------------------
   -- Available_Write                                                    --
   ------------------------------------------------------------------------
   --  Unlimited
   function Available_Write (This: in Stream_Type)
      return Stream_Element_Count;

private

   type Stream_Type
   is new Agpl.Streams.Controlled.Stream_Type with record
      Back      : Agpl.Streams.Stream_Access;
   end record;

end Agpl.Streams.Filter;
