 

package body Agpl.Monitor is

   protected body Semaphore is

      entry P when True is
      begin
         if Caller = P'Caller then
            In_use := In_use + 1;
         elsif In_use = 0 then
            Caller := P'Caller;
            In_use := 1;
         else
            requeue Safe_P with abort;
         end if;
      end P;

      entry Safe_P when In_use = 0 is
      begin
         Caller := Safe_P'Caller;
         In_use := 1;
      end Safe_P;

      entry V when True is
      begin
         if V'Caller /= Caller then
            raise Use_error;
         else
            In_use := In_use - 1;
            if In_use = 0 then
               Caller := Null_task_id;
            end if;
         end if;
      end V;

   end Semaphore;

   ----------------
   -- Initialize --
   ----------------
   --  Get
   procedure Initialize(this: in out Object) is
   begin
      this.S.P;
   end Initialize;


   --------------
   -- Finalize --
   --------------
   --  Release
   procedure Finalize(this: in out Object) is
   begin
      this.S.V;
   end Finalize;

end Agpl.Monitor;
