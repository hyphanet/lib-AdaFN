 

--  An observable stream can tell in advance how many data you can read or
--  write into it.
--  Additionally, it is derived from Controlled so you could provide
--  initialization and finalization for it.

with Agpl.Streams.Controlled;

package Agpl.Streams.Observable is

   pragma Preelaborate;

   ------------------------------------------------------------------------
   -- Stream_type                                                        --
   ------------------------------------------------------------------------
   type Stream_Type
   is abstract new Agpl.Streams.Controlled.Stream_Type with private;

   type Stream_Access is access all Stream_type'Class;

   ------------------------------------------------------------------------
   -- Available_Read                                                     --
   ------------------------------------------------------------------------
   function Available_Read (This : in Stream_Type)
      return Stream_Element_Count is abstract;
   pragma Inline (Available_Read);

   ------------------------------------------------------------------------
   -- Available_Write                                                    --
   ------------------------------------------------------------------------
   function Available_Write (This: in Stream_Type)
      return Stream_Element_Count is abstract;
   pragma Inline (Available_Write);

private

   type Stream_Type
   is abstract new Agpl.Streams.Controlled.Stream_Type with null record;

end Agpl.Streams.Observable;
