 

with Agpl.Trace;

with Ada.Calendar;
with Ada.Finalization;

package Agpl.Debug.Timer is

   pragma Elaborate_Body;

   Deadline_Failed : exception;

   Unknown_Context : aliased String := "Unknown context";

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   --  Declare at the context you want to ensure it lasts less than Deadline.
   --  If deadline is missed, upon exit of the context a trace will be generated.
   --  If Trace = Null_Object, then instead a Deadline_Failed will be raised.
   --  If Always_Raise, the exception will be raised even if the trace is generated.
   --  Give deadline in milliseconds.
   type Object (
      Id           : access String            := Unknown_Context'Access;
      Trace        : Agpl.Trace.Object_Access := Agpl.Trace.Null_Object;
      Deadline     : Natural                  := 10_000;
      Always_Raise : Boolean                  := False)
   is limited private;

private

   type Object (
      Id           : access String            := Unknown_Context'Access;
      Trace        : Agpl.Trace.Object_Access := Agpl.Trace.Null_Object;
      Deadline     : Natural                  := 10_000;
      Always_Raise : Boolean                  := False) is
   new Ada.Finalization.Limited_Controlled with
   record
      Start : Ada.Calendar.Time := Ada.Calendar.Clock;
   end record;

   procedure Finalize (This : in out Object);

end Agpl.Debug.Timer;
