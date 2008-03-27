 

--  Efficient event queue. Useful for timeouts, as an example.

with Agpl.Trace; use Agpl.Trace;

with Ada.Unchecked_deallocation;
use  Ada;
with Ada.Text_IO; use Ada.Text_IO;

package body Agpl.Event_queues.Calendar is

   procedure Free is new Unchecked_deallocation (
      Context_type'Class, Context_access);

   --  Priority
   function Less (L, R : in Event_type) return Boolean is
   begin
      return L.Deadline < R.Deadline or else
         (L.Deadline = R.Deadline and L.Id < R.Id);
   end Less;

   --  Equal (by id)
   function Equal (L, R : in Event_type) return Boolean is
   begin
      return L.Id = R.Id;
   end Equal;

   -----------------
   -- Has_Expired --
   -----------------

   function Has_Expired   (This : in Event_Type) return Boolean is
   begin
      return This.Context /= null;
   end Has_Expired;

   -------------------
   -- Get_Remaining --
   -------------------

   function Get_Remaining (This : in Event_Type) return Duration is
   begin
      return This.Deadline - Clock;
   end Get_Remaining;

   ------------------
   -- Get_Deadline --
   ------------------

   function Get_Deadline (Event : in Event_Type) return Ada.Calendar.Time is
   begin
      return Event.Deadline;
   end Get_Deadline;

   ------------
   -- Create --
   ------------

   procedure Create (
      This     : in out Object;
      Event    : out    Event_type;
      Deadline : in     Time;
      Action   : in     Action_procedure;
      Context  : in     Context_type'Class) is
   begin
      This.Seq.Get_next (Event.Id);
      Event.Deadline  := Deadline;
      Event.Action    := Action;
      Event.Context   := new Context_type'Class'(Context);
      This.List.Insert (Event);
      if not This.Waiter'Terminated then
         This.Waiter.Reschedule (New_event);
      end if;
   end Create;

   ------------
   -- Cancel --
   ------------

   procedure Cancel (
      This     : in out Object;
      Event    : in out Event_type) is
      Found    : Boolean;
   begin
      if Event.Context /= null then
         This.List.Get_remove (Event, Found);
         if Found then
            Free (Event.Context);
         end if;
      end if;
   end Cancel;

   -----------------------
   -- Get_Next_Deadline --
   -----------------------

   function Get_Next_Deadline (This : in Object) return Time is
   begin
      return This.Next_Deadline.Get;
   end Get_Next_Deadline;

   -----------------------
   -- Get_Master_Status --
   -----------------------

   function Get_Master_Status (This : in Object) return Master_States is
   begin
      return This.Master_Status;
   end Get_Master_Status;

   -----------------------
   -- Get_Worker_Status --
   -----------------------

   function Get_Worker_Status (This : in Object) return Worker_States is
   begin
      return This.Worker_Status;
   end Get_Worker_Status;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (This : in out Object) is
   begin
      This.Waiter.Shutdown;
   end Shutdown;

   --------------
   -- Is_empty --
   --------------

   function Is_empty (This : in Object) return Boolean is
   begin
      return This.List.Is_empty;
   end Is_empty;

   ------------
   -- Length --
   ------------

   function Length (This : in Object) return Natural is
   begin
      return This.List.Length;
   end Length;

   -------------
   -- Suspend --
   -------------

   procedure Suspend (This : in Object) is
   begin
      This.Waiter.Reschedule (Suspend);
   end Suspend;

   procedure Resume  (This : in Object) is
   begin
      This.Waiter.Reschedule (Resume);
   end Resume;

   -------------------
   -- Active_Object --
   -------------------

   task body Active_object is
      Next         : Event_type;
      Deadline     : Time;
      Found        : Boolean;
      Worker_Ready : Boolean := True;
      Suspended    : Boolean := False;
   begin
      loop
         begin
            --  Deadline triggered or rescheduling (new event)
            if Suspended then
               Deadline := End_Of_Time;
               Parent.Next_Deadline.Set (End_Of_Time);
               Parent.Master_Status := Idle;
            else
               Parent.List.Get_First_Remove (Next, Found);
               if not Found then
                  Deadline := End_Of_Time;
                  Parent.Next_Deadline.Set (End_Of_Time);
                  Parent.Master_Status := Idle;
               else
                  Parent.Next_Deadline.Set (Next.Deadline);
                  if Next.Deadline <= Clock then
                     --  Run it if possible
                     if Worker_Ready then
                        Parent.Master_Status := Executing;
                        Parent.Doer.Execute (Next);
                        Worker_Ready := False;
                        Deadline     := Clock; -- So we'll peek at the next event right now.
                     else
                        --  Busy. Delay until worker signals us:
                        Parent.Master_Status := Waiting_Worker;
                        Deadline := End_Of_Time;
                        Parent.List.Insert (Next); -- Will go to head again.
                     end if;
                  else
                     --  Reinsert it since it must not run still.
                     Parent.Master_Status := Waiting_Deadline;
                     Deadline := Next.Deadline;
                     Parent.List.Insert (Next);
                  end if;
               end if;
            end if;

            --  To aid in automatic finalization, if no events pending...
            if Deadline = End_Of_Time then
               --  Wait for news
               select
                  accept Reschedule (Action : in Action_Type) do
                     case Action is
                        when Job_Finished =>
                           Worker_Ready := True;
                           Parent.Master_Status := Ready;
                        when Suspend =>
                           Suspended := True;
                        when Resume =>
                           Suspended := False;
                        when New_Event =>
                           null;
                     end case;
                  end Reschedule;
               or
                  accept Shutdown;
                  exit;
               or
                  terminate;
               end select;
            else
               --  Wait for news
               select
                  accept Reschedule (Action : in Action_Type) do
                     case Action is
                        when Job_Finished =>
                           Worker_Ready := True;
                           Parent.Master_Status := Ready;
                        when Suspend =>
                           Suspended := True;
                        when Resume =>
                           Suspended := False;
                        when New_Event =>
                           null;
                     end case;
                  end Reschedule;
               or
                  accept Shutdown;
                  exit;
               or
                  delay until Deadline;
               end select;
            end if;
         exception
            when E : others =>
               Log ("Event_Queue.Calendar.Active: " & Trace.Report (E), Trace.Error);
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
               Parent.Worker_Status := Executing;
               begin
                  Action (Context);
               exception
                  when E : others =>
                     Put_Line ("Event_Queue.Calendar.Worker [action]: " & Trace.Report (E));
                     Trace.Log ("Event_Queue.Calendar.Worker [action]: " &
                                Trace.Report (E),
                                Trace.Error);
               end;
               Parent.Worker_Status := Waiting;
               Free (Context);
               if not Parent.Waiter'Terminated then
                  Parent.Waiter.Reschedule (Job_finished);
               end if;
            or
               terminate;
            end select;
         exception
            when E : others =>
               Trace.Log (
                  "Event_Queue.Calendar.Worker [no-action]: " & Trace.Report (E),
                  Trace.Error);
         end;
      end loop;
   end Worker;

end Agpl.Event_queues.Calendar;
