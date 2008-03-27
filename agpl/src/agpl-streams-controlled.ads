 

--  A stream which has a finalization and initialization method.

with Ada.Finalization;
with Ada.Streams;

package Agpl.Streams.Controlled is

   pragma Preelaborate;

   ------------------------------------------------------------------------
   -- Stream_type                                                        --
   ------------------------------------------------------------------------
   type Stream_Type
   is abstract new Ada.Streams.Root_Stream_Type with private;

   type Stream_Access is access all Stream_type'Class;

   ------------------------------------------------------------------------
   -- Finalize                                                           --
   ------------------------------------------------------------------------
   --  Called on destruction of the stream
   --  Default one does nothing.
   procedure Finalize (This : in out Stream_Type);

   ------------------------------------------------------------------------
   -- Initialize                                                         --
   ------------------------------------------------------------------------
   --  Called on creation of the stream
   --  Default one does nothing.
   procedure Initialize (This : in out Stream_Type);

private

   type Controller_Type (Parent : access Stream_Type'Class) is new
   Ada.Finalization.Limited_Controlled with null record;

   procedure Initialize (This : in out Controller_Type);
   procedure Finalize   (This : in out Controller_Type);

   type Stream_Type
   is abstract new Ada.Streams.Root_Stream_Type with record
      Controller : Controller_Type (Stream_Type'Access);
   end record;

end Agpl.Streams.Controlled;
