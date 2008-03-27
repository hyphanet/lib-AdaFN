 

with Text_Io;

package body Agpl.Debug.Timer is

   procedure Finalize (This : in out Object) is
      use Ada.Calendar;
      use type Agpl.Trace.Object_Access;
      Elapsed : constant Duration := Clock - This.Start;
   begin
      if Elapsed > (Duration (This.Deadline) / 1000.0) then
         Text_Io.Put_Line ("Deadline failed");
         if This.Trace /= Agpl.Trace.Null_Object then
            Agpl.Trace.Log (This.Trace.all, 
               "Tracer: " & This.Id.all & ": Elapsed" &
               Duration'Image (Elapsed) & " > " & 
               Duration'Image (Duration (This.Deadline) / 1000.0), 
               Agpl.Trace.Error);
         end if;
         if This.Always_Raise or else This.Trace = Agpl.Trace.Null_Object then
            raise Deadline_Failed;
         end if;
      end if;
   end Finalize;

end Agpl.Debug.Timer;
