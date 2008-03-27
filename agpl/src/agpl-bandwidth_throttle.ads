 

--  Class for bandwidth throttling.
--  Provides no arbitration; first come first served.

with Ada.Streams;
with Ada.Calendar;
use  Ada;

package Agpl.Bandwidth_Throttle is

   pragma Elaborate_Body;

   --  Bandwidth is the elements/second to allow
   --  Period is the period for feedback. Longer ones allow the use
   --    of unused bandwidth for more time. (In milliseconds)
   protected type Object (
      Bandwidth : Ada.Streams.Stream_Element_Count;
      Period    : Positive)
   is
      --  Gives the available bandwidth at this moment (real + extra):
      procedure Available (Bandwidth : out Ada.Streams.Stream_Element_Count);

      --  Issue a bandwidth petition. Awarded can be less that solicited.
      --  Awarded will never be more than Natural'Last / 2 so you can
      --    add Awarded + Awarded_Extra.
      --  Extra flag requests bandwidth from unused past cycles.
      --  Past cycles will faint in exponential fashion.
      procedure Commit (
         Desired : in     Ada.Streams.Stream_Element_Count;
         Awarded :    out Ada.Streams.Stream_Element_Count;
         Extra   : in     Boolean := False);

   private

      Gap      : Duration      := Duration (Period) / 1000.0;

      Remanent : Ada.Streams.Stream_Element_Count := 0; -- Remanent in this cycle
      Unused   : Ada.Streams.Stream_Element_Count := 0; -- Remanent from past cycles
      Last_Req : Calendar.Time                    := Calendar.Clock;

   end Object;

   type Object_access is access all Object;

   Unlimited : aliased Object (Ada.Streams.Stream_Element_Count'Last, 1000);

end Agpl.Bandwidth_Throttle;
