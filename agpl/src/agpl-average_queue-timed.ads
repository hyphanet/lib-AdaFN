 

with Ada.Calendar;
with Ada.Finalization;
use  Ada;

generic
package Agpl.Average_queue.Timed is

   pragma Elaborate_Body;

   --  This object is thread safe:
   --  Number of averaging slots, and how many milliseconds these take:
   type Object (Slots : Positive := 12; Slot_duration : Positive := 5000)
   is limited private;
   type Object_Access is access all Object;

   ------------------------------------------------------------------------
   -- Extended_Push                                                      --
   ------------------------------------------------------------------------
   --  Gives extra info: If a gap change has happened and how many empty
   --  gaps after it have happened:
   procedure Extended_Push (
      This       : in out Object;
      Data       : in     Item;
      Gap_Change : out Boolean;  -- True if at least a new gap has been pushed
      Empty_Gaps : out Natural); -- Number of empty gaps after the last one pushed
                                 --  and before the current one.

   ------------------------------------------------------------------------
   -- Push                                                               --
   ------------------------------------------------------------------------
   procedure Push (This : in out Object; Data : in Item);

   ------------------------------------------------------------------------
   -- Average                                                            --
   ------------------------------------------------------------------------
   procedure Average (This : in out Object; Result : out Item);

   ------------------------------------------------------------------------
   -- Free                                                               --
   ------------------------------------------------------------------------
   procedure Free (This : in out Object_Access);

private

   use type Calendar.Time;


   protected type Safe_object (Slots : Positive; Slot_duration : Positive) is
      procedure Push (Value  : in  Item; Gap_Change : out Boolean; Empty_Gaps : out Natural);
      procedure Avg  (Result : out Item);
      procedure Create;
      procedure Destroy;
   private
      --  Constant:
      Gap        : Duration      := Duration (Slot_duration) / 1000.0;

      Slot_start : Calendar.Time := Calendar.Clock;
      Acum       : Item          := 0.0;
      Data       : Average_queue.Object_access;
   end Safe_object;

   type Object (Slots : Positive := 12; Slot_duration : Positive := 5000) is
      new Finalization.Limited_Controlled with
      record
         Safe       : Safe_object (Slots, Slot_duration);
      end record;

   procedure Initialize (This : in out Object);
   procedure Finalize   (This : in out Object);

   pragma Inline (Push, Average);

end Agpl.Average_queue.Timed;
