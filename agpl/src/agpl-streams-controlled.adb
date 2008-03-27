 

--  A stream which has a finalization and initialization method.

package body Agpl.Streams.Controlled is

   ------------------------------------------------------------------------
   -- Finalize                                                           --
   ------------------------------------------------------------------------
   --  Called on destruction of the stream
   --  Default one does nothing.
   procedure Finalize (This : in out Stream_Type) is
      pragma Unreferenced (This);
   begin
      null;
   end Finalize;

   ------------------------------------------------------------------------
   -- Initialize                                                         --
   ------------------------------------------------------------------------
   --  Called on creation of the stream
   --  Default one does nothing.
   procedure Initialize (This : in out Stream_Type) is
      pragma Unreferenced (This);
   begin
      null;
   end Initialize;

   procedure Initialize (This : in out Controller_Type) is
   begin
      Initialize (This.Parent.all);
   end Initialize;

   procedure Finalize   (This : in out Controller_Type) is
   begin
      Finalize (This.Parent.all);
   end Finalize;

end Agpl.Streams.Controlled;
