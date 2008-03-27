with Agpl.Trace; use Agpl.Trace;

with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Task_Identification; use Ada.Task_Identification;
with Ada.Task_Termination;    use Ada.Task_Termination;

package body Agpl.Task_Termination is

   protected Object is

      procedure Grim_Reaper (Cause : Cause_Of_Termination;
                             T     : Ada.Task_Identification.Task_Id;
                             X     : Ada.Exceptions.Exception_Occurrence);

   end Object;

   protected body Object is
      procedure Grim_Reaper (Cause : Cause_Of_Termination;
                             T     : Ada.Task_Identification.Task_Id;
                             X     : Ada.Exceptions.Exception_Occurrence)
      is
         Levels : constant array (Cause_Of_Termination) of Trace.Levels :=
                    (Normal => Debug, others => Error);
      begin
         Log ("Grim reaper: Task [" & Image (T) & "] finished with cause " &
              Cause_Of_Termination'Image (Cause),
              Levels (Cause), Log_Section);
         if Cause = Unhandled_Exception then
            Log ("Grim reaper: Task [" & Image (T) & "] exception was " &
              Trace.Report (X),
              Levels (Cause), Log_Section);
         end if;
      end Grim_Reaper;
   end Object;

begin
   Set_Dependents_Fallback_Handler (Object.Grim_Reaper'Access);
end Agpl.Task_Termination;
