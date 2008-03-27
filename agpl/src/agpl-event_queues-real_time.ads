 

--  Efficient event queue. Useful for timeouts, as an example.

--  Implemented with tagged types. That makes genericity unnecesary. A queue
--   can perform multiple kind of events.

with Agpl.Protected_sorted_index;
with Agpl.Sequence;
with Agpl.Trace;

with Ada.Real_time;
use  Ada.Real_time;

package Agpl.Event_queues.Real_Time is

   pragma Elaborate_Body;

   --  Handle for an event. Can be used to cancel it:
   type Event_type is private;

   type Object (
      Stack_size : Natural             := 64 * 1024;
      Tracer     : Trace.Object_Access := Trace.Null_Object) is limited private;
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

   --  Pending events?
   function Is_empty (This : in Object) return Boolean;

   function Length (This : in Object) return Natural;

   procedure Shutdown (This : in out Object);

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

   type Action_type is (New_event, Job_finished);

   task type Active_object (Parent : access Object) is
      entry Reschedule (Action : in Action_type);
      entry Shutdown;
   end Active_object;

   task type Worker (Parent : access Object) is
      pragma Storage_size (Parent.Stack_size);
      entry Execute (Event : in Event_type);
   end Worker;

   type Object (
      Stack_size : Natural             := 64 * 1024;
      Tracer     : Trace.Object_Access := Trace.Null_Object) is
   record
      List   : Event_list.Sorted_index;
      Seq    : Id_sequence.Object;
      Waiter : Active_object (Object'Access);
      Doer   : Worker (Object'Access);
   end record;

end Agpl.Event_queues.Real_Time;
