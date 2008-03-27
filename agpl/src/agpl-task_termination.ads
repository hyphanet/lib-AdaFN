package Agpl.Task_Termination is

   pragma Elaborate_Body;

   Log_Section : constant String := "agpl.task_termination";

   --  To with this package will cause the installation of a default termination
   --  handler that will task terminations via Agpl.Trace.
   --  Normal terminations will be reported at debug level,
   --  while others at warning level.

end Agpl.Task_Termination;
