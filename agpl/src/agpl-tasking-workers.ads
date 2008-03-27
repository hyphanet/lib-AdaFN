with Agpl.Tasking.Code;

package Agpl.Tasking.Workers is

   pragma Preelaborate;

   procedure Launch (This     : Code.Object'Class;
                     Class    : String  := "";
                     Stack    : Natural := 64 * 1024;
                     Reap_Old : Boolean := True);
   --  A copy will be made of This for internal use.
   --  If Reap_Old, then after launching the new worker, an attemp will be
   --  made at freeing old finished workers.
   --  Class is for grouping counts only

   function Count return Natural;
   --  O (1)

   function Class_Count (Class : String) return Natural;
   --  O (log n)

   procedure Purge_Old;
   --  O (n)
   --  Free memory for old finished workers.

end Agpl.Tasking.Workers;
