 

with Ada.Calendar;
with Ada.Streams;

--  A Time can't be streamed to remote machines. This is an extension of
--  regular timestamps that replaces 'Read and 'Write with proper
--  serialization subprograms.

package Agpl.Calendar.Serializable_Time is

   --   pragma Elaborate_Body;

   type Object is new Ada.Calendar.Time;

   function "+" (This : in Object) return Ada.Calendar.Time;
   pragma Inline ("+");

   function "+" (This : in Ada.Calendar.Time) return Object;
   pragma Inline ("+");

   procedure Read (Stream : access Ada.Streams.Root_Stream_Type'Class;
                   This   :    out Object);
   for Object'Read use Read;

   procedure Write (Stream : access Ada.Streams.Root_Stream_Type'Class;
                    This   : in     Object);
   for Object'Write use Write;

private

   use Ada.Calendar;

   Epoch : constant Object := Time_Of (1970, 1, 1, 0.0);

end Agpl.Calendar.Serializable_Time;
