 

--  This object will force a delay if no enough time has elapsed.

with Ada.Calendar;
use  Ada;

package Agpl.Delayer is

   pragma Elaborate_Body;

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   --  Interval in milliseconds
   protected type Object (Interval : Natural) is

      -------------
      -- Request --
      -------------
      --  Will return when the next slot of time has been reached.
      procedure Request;

   private

      Next_run : Calendar.Time := Calendar.Clock;
      Gap      : Duration      := Duration (Interval) / 1000.0;

   end Object;

end Agpl.Delayer;
