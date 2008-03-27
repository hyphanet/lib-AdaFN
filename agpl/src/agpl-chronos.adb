 

with Agpl.Calendar.Format;
with Agpl.Trace; use Agpl.Trace;

package body Agpl.Chronos is

   use type Ada.Calendar.Time;

   ------------------------------------------------------------------------
   -- Reset                                                              --
   ------------------------------------------------------------------------
   procedure Reset (This : in out Object; Elapsed : Duration := 0.0) is
   begin
      This.Start := Ada.Calendar.Clock - Elapsed;
   end Reset;

   ------------------------------------------------------------------------
   -- Elapsed                                                            --
   ------------------------------------------------------------------------
   function Elapsed (This : in Object) return Duration is
   begin
      return Ada.Calendar.Clock - This.Start;
   exception
      when E : others =>
         Log ("Chronos: " & Report (E), Error);
         Log ("Chronos: Why???", Error);
         return This.Elapsed; -- Try again!
   end Elapsed;

   ------------------------------------------------------------------------
   -- Image                                                              --
   ------------------------------------------------------------------------
   function Image (This : in Object) return String is
   begin
      return Agpl.Calendar.Format.Image (Elapsed (This));
   end Image;

   -----------
   -- Clock --
   -----------

   function Clock return Object is
   begin
      return (Start => Ada.Calendar.Clock);
   end Clock;

   -----------
   -- Value --
   -----------

   function Value (This : in Object) return Ada.Calendar.Time is
   begin
      return This.Start;
   end Value;

end Agpl.Chronos;
