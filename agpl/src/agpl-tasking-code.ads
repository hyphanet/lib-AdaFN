package Agpl.Tasking.Code is

   pragma Pure;

   type Object is interface;
   --  Non-limited because a copy is needed later

   procedure Init (This : in out Object) is null;
   --  Called before execution starts

   procedure Run (This : in out Object) is null;
   --  override Run with your desired code to be executed in a new task

   procedure Destroy (This : in out Object) is null;
   --  Called upon Run finalization.

end Agpl.Tasking.Code;
