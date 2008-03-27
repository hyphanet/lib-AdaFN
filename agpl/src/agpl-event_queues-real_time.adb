 

--  Efficient event queue. Useful for timeouts, as an example.

with Ada.Unchecked_Deallocation; use Ada;

package body Agpl.Event_queues.Real_Time is

   procedure Free is new Unchecked_Deallocation (
      Context_type'Class,
      Context_access);

   --  Priority
   function Less (L, R : in Event_type) return Boolean is
   begin
      return L.Deadline < R.Deadline
            or else (L.Deadline = R.Deadline and L.Id < R.Id);
   end Less;

   --  Equal (by id)
   function Equal (L, R : in Event_type) return Boolean is
   begin
      return L.Id = R.Id;
   end Equal;

   --  Create an avent
   procedure Create
     (This     : in out Object;
      Event    : out Event_type;
      Deadline : in Time;
      Action   : in Action_procedure;
      Context  : in Context_type'Class)
   is
   begin
      This.Seq.Get_next (Event.Id);
      Event.Deadline := Deadline;
      Event.Action   := Action;
      Event.Context  := new Context_type'Class'(Context);
      This.List.Insert (Event);
      This.Waiter.Reschedule (New_event);
   end Create;

   procedure Cancel (This : in out Object; Event : in out Event_type) is
      Found : Boolean;
   begin
      if Event.Context /= null then
         This.List.Get_remove (Event, Found);
         if Found then
            Free (Event.Context);
         end if;
      end if;
   end Cancel;

   procedure Shutdown (This : in out Object) is
   begin
      This.Waiter.Shutdown;
   end Shutdown;

   --  Pending events?
   function Is_empty (This : in Object) return Boolean is
   begin
      return This.List.Is_empty;
   end Is_empty;

   function Length (This : in Object) return Natural is
   begin
      return This.List.Length;
   end Length;

   task body Active_object is
      Next         : Event_type;
      Deadline     : Time;
      Found        : Boolean;
      Worker_ready : Boolean := True;
   begin
      loop
         begin
         --  Deadline triggered or rescheduling (new event)
            Parent.List.Get_first_remove (Next, Found);
            if not Found then
               Deadline := Clock + To_Time_Span (60.0);
            else
               if Next.Deadline <= Clock then
                  --  Run it if possible
                  if Worker_ready then
                     Parent.Doer.Execute (Next);
                     Worker_ready := False;
                     Deadline     := Clock;
                  else
                     --  Busy. Delay until him signals us:
                     Deadline := Clock + To_Time_Span (60.0);
                     Parent.List.Insert (Next);
                  end if;
               else
                  --  Reinsert it
                  Deadline := Next.Deadline;
                  Parent.List.Insert (Next);
               end if;
            end if;
            --  Wait for news
            select
               accept Reschedule (Action : in Action_type) do
                  if Action = Job_finished then
                     Worker_ready := True;
                  end if;
               end Reschedule;
            or
               accept Shutdown;
               exit;
            or
               delay until Deadline;
            end select;
         exception
            when E : others =>
               Trace.Log
                 (Parent.Tracer,
                  "Event_queue.Active_object: " & Trace.Report (E),
                  Trace.Error);
         end;
      end loop;
   end Active_object;

   task body Worker is
      Context : Context_access;
      Action  : Action_procedure;
   begin
      loop
         begin
            select
               accept Execute (Event : in Event_type) do
                  Context := Event.Context;
                  Action  := Event.Action;
               end Execute;
               begin
                  Action (Context);
               exception
                  when E : others =>
                     Trace.Log
                       (Parent.Tracer,
                        "Event_queue.Worker [action]: " & Trace.Report (E),
                        Trace.Error);
               end;
               Free (Context);
               Parent.Waiter.Reschedule (Job_finished);
            or
               terminate;
            end select;
         exception
            when E : others =>
               Trace.Log
                 (Parent.Tracer,
                  "Event_queue.Worker [no-action]: " & Trace.Report (E),
                  Trace.Error);
         end;
      end loop;
   end Worker;

end Agpl.Event_queues.Real_Time;
