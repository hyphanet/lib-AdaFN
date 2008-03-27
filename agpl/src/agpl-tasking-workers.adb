with Ada.Containers.Doubly_Linked_Lists,
     Ada.Finalization,
     Ada.Unchecked_Deallocation,
     Agpl.Counter.Multi,
     Agpl.Tasking.Code.Handle,
     Agpl.Trace,
     Agpl.Types.Ustrings;

use Agpl.Trace,
    Agpl.Types.Ustrings;

package body Agpl.Tasking.Workers is

   Class_Counter : Agpl.Counter.Multi.Object;

   task type Worker (Stack_Size : Natural) is
      pragma Storage_Size (Stack_Size);

      entry Set_Class (Class : String);
      entry Launch (This : Code.Object'Class);
   end Worker;

   type Worker_Access is access Worker;

   procedure Free is new Ada.Unchecked_Deallocation (Worker, Worker_Access);

   package Worker_Lists is new
     Ada.Containers.Doubly_Linked_Lists (Worker_Access);

   protected Safe is
      procedure Increment;
      procedure Decrement;

      procedure Append (W : Worker_Access);
      procedure Check_Old;
      function  Count return Natural;
   private
      Counter         : Natural := 0;
      Running_Workers : Worker_Lists.List;
   end Safe;

   type Autocounter is new Ada.Finalization.Limited_Controlled with null record;
   procedure Initialize (This : in out Autocounter);
   procedure Finalize   (This : in out Autocounter);

   procedure Initialize (This : in out Autocounter) is
      pragma Unreferenced (This);
   begin
      Safe.Increment;
   end Initialize;

   procedure Finalize   (This : in out Autocounter) is
      pragma Unreferenced (This);
   begin
      Safe.Decrement;
   end Finalize;

   ----------
   -- Safe --
   ----------

   protected body Safe is

      ------------
      -- Append --
      ------------

      procedure Append (W : Worker_Access) is
      begin
         Running_Workers.Append (W);
      end Append;

      ---------------
      -- Check_Old --
      ---------------

      procedure Check_Old is
         use Worker_Lists;
         I : Cursor := Worker_Lists.Last (Running_Workers);
         J : Cursor;
      begin
         while Has_Element (I) loop
            J := Previous (I);

            declare
               W : Worker_Access := Element (I);
            begin
               if W'Terminated then
                  Free (W);
                  Running_Workers.Delete (I);
               end if;
            end;

            I := J;
         end loop;
      end Check_Old;

      -----------
      -- Count --
      -----------

      function Count return Natural is
      begin
         return Counter;
      end Count;

      ---------------
      -- Decrement --
      ---------------

      procedure Decrement is
      begin
         Counter := Counter - 1;
      end Decrement;

      ---------------
      -- Increment --
      ---------------

      procedure Increment is
      begin
         Counter := Counter + 1;
      end Increment;

   end Safe;

   -----------
   -- Count --
   -----------

   function Count return Natural is
   begin
      return Safe.Count;
   end Count;

   ------------
   -- Worker --
   ------------

   task body Worker is
      C : Code.Handle.Object;
      A : Autocounter; pragma Unreferenced (A);

      Class : Ustring;
   begin

      accept Set_Class (Class : String) do
         Worker.Class := +Class;
      end Set_Class;

      Class_Counter.Add (+Class);

      accept Launch (This : Code.Object'Class) do
         C.Set (This);
      end Launch;

      begin
         C.Ref.all.Init;
      exception
         when E : others =>
            Log ("Agpl.Tasking.Worker [init]: " &
                 External_Tag (C.Ref.all'Tag) & ": " & Report (E),
                 Error);
      end;

      begin
         C.Ref.all.Run;
      exception
         when E : others =>
            Log ("Agpl.Tasking.Worker [run]: " &
                 External_Tag (C.Ref.all'Tag) & ": " & Report (E),
                 Error);
      end;

      begin
         C.Ref.all.Destroy;
      exception
         when E : others =>
            Log ("Agpl.Tasking.Worker [destroy]: " &
                 External_Tag (C.Ref.all'Tag) & ": " & Report (E),
                 Error);
      end;

      Class_Counter.Add (+Class, -1);

   exception
      when E : others =>
         Log ("Agpl.Tasking.Worker: " & Report (E), Error);
         raise;
   end Worker;

   ------------
   -- Launch --
   ------------

   procedure Launch
     (This     : Code.Object'Class;
      Class    : String  := "";
      Stack    : Natural := 64 * 1024;
      Reap_Old : Boolean := True)
   is
      W : constant Worker_Access := new Worker (Stack);
   begin
      W.Set_Class (Class);
      W.Launch (This);

      Safe.Append (W);

      if Reap_Old then
         Safe.Check_Old;
      end if;
   end Launch;

   ---------------
   -- Purge_Old --
   ---------------

   procedure Purge_Old is
   begin
      Safe.Check_Old;
   end Purge_Old;

   -----------------
   -- Class_Count --
   -----------------

   function Class_Count (Class : String) return Natural is
   begin
      return Class_Counter.Val (Class);
   end Class_Count;

end Agpl.Tasking.Workers;
