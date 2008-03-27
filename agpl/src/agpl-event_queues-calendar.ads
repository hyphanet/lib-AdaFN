 

--  Efficient event queue. Useful for timeouts, as an example.

--  Implemented with tagged types. That makes genericity unnecesary. A queue
--   can perform multiple kind of events.

with Agpl.Protected_sorted_index;
with Agpl.Protected_Values.Time;
with Agpl.Sequence;

with Ada.Calendar;
use  Ada.Calendar;

package Agpl.Event_queues.Calendar is

   pragma Elaborate_Body;

   --  Handle for an event. Can be used to cancel it:
   type Event_type is private;

   --  Called: (not if deadline arrived, may be late)
   function Has_Expired   (This : in Event_Type) return Boolean;
   --  Remaining time to trigger (may be negative if late)
   function Get_Remaining (This : in Event_Type) return Duration;
   --  Deadline:
   function Get_Deadline  (Event : in Event_Type) return Ada.Calendar.Time;

   type Object (Stack_size : Natural := 64 * 1024) is limited private;
   type Object_access is access all Object;

   --  Create an event
   procedure Create (
      This     : in out Object;
      Event    : out    Event_type;
      Deadline : in     Time;
      Action   : in     Action_procedure;
      Context  : in     Context_type'Class);

   procedure Cancel (
      This     : in out Object;
                     Event    : in out Event_type);

   function Get_Next_Deadline (This : in Object) return Time;
   --  Next planned event, or Time'Last if no one.

   function Get_Master_Status (This : in Object) return Master_States;

   function Get_Worker_Status (This : in Object) return Worker_States;

   --  Pending events?
   function Is_empty (This : in Object) return Boolean;

   function Length (This : in Object) return Natural;

   procedure Shutdown (This : in out Object);
   --  Premature termination (if events pending).
   --  If no events pending, termination should be automatic

   procedure Suspend (This : in Object);
   procedure Resume  (This : in Object);
   --  Suspend/Resume execution of events
   --  On resume, all expired events will be executed.

   End_Of_Time : constant Time := Time_Of (Year_Number'Last, 1, 1);
   --  Returned by Get_Next_Deadline when no other deadline exists.

private

   --  Uses timestamp
   function Less  (L, R : in Event_type) return Boolean;
   --  Uses Id.
   function Equal (L, R : in Event_type) return Boolean;
   pragma Inline (Less, Equal);

   --  Maximum simultaneous pending events:
   type Id_type is mod 2 ** 32;

   package Id_sequence is new Sequence (Id_type);

   type Event_type is record
      Deadline : Time;
      Id       : Id_type;
      Action   : Action_procedure;
      Context  : Context_access;
   end record;

   package Event_list is new
      Protected_sorted_index (Event_type, Less, Equal);

   type Action_type is (New_event, Job_Finished, Suspend, Resume);

   task type Active_object (Parent : access Object) is
      entry Reschedule (Action : in Action_type);
      entry Shutdown;
   end Active_object;

   task type Worker (Parent : access Object;
                     Stack  : Natural) is
      pragma Storage_size (Stack);
      entry Execute (Event : in Event_type);
   end Worker;

   type Object (Stack_size : Natural := 64 * 1024) is
      limited record
         List   : Event_list.Sorted_index;
         Seq    : Id_sequence.Object;
         Waiter : Active_object (Object'Access);
         Doer   : Worker (Object'Access, Stack_Size);

         Master_Status : Master_States := Idle;
         pragma Atomic (Master_Status);

         Worker_Status : Worker_States := Waiting;
         pragma Atomic (Worker_Status);

         Next_Deadline : Protected_Values.Time.Object;
   end record;

end Agpl.Event_queues.Calendar;
