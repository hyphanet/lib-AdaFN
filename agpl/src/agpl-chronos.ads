 

--  Chronometer. Starts counting on creation.

with Ada.Calendar;

package Agpl.Chronos is

   pragma Elaborate_Body;

   type Object is tagged private;
   type Object_Access is access Object'Class;

   ------------------------------------------------------------------------
   -- Reset                                                              --
   ------------------------------------------------------------------------
   procedure Reset (This : in out Object; Elapsed : Duration := 0.0);
   pragma Inline (Reset);

   ------------------------------------------------------------------------
   -- Elapsed                                                            --
   ------------------------------------------------------------------------
   function Elapsed (This : in Object) return Duration;
   pragma Inline (Elapsed);

   ------------------------------------------------------------------------
   -- Image                                                              --
   ------------------------------------------------------------------------
   function Image (This : in Object) return String;

   function Value (This : in Object) return Ada.Calendar.Time;
   pragma Inline (Value);
   --  This start time of this object

   function Clock return Object; -- An object denoting current time
   pragma Inline (Clock);

private

   type Object is tagged record
      Start : Ada.Calendar.Time := Ada.Calendar.Clock;
   end record;

end Agpl.Chronos;
