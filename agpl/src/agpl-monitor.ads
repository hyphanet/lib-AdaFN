 

--  Mutex with counter. A task may safely request it multiple times,
--  as long as it releases it the same times

with Ada.Finalization;
with Ada.Task_identification;

package Agpl.Monitor is

   pragma Preelaborate;

   use Ada;
   use Ada.Task_identification;

   Use_error : exception; -- Should never happen.

   protected type Semaphore is
      entry P;
      entry V;
   private
      entry Safe_P;

      Caller : Task_id := Null_task_id;           -- Requester
      In_use : Natural := 0;                      -- Times requested
   end Semaphore;

   type Semaphore_access is access all Semaphore;

   --  The following object is defined for conveniently usage of semaphores.
   --  Use:
   --  S : aliased Semaphore;
   --  declare
   --    M : Object (S'access);
   --  begin
   --    Exclusive_work;
   --  end;
   type Object (S : access Semaphore) is new
      Finalization.Limited_Controlled with null record;

   procedure Initialize (this : in out Object);
   procedure Finalize   (this : in out Object);

private

   pragma Inline (Initialize, Finalize);

end Agpl.Monitor;
