 

with Ada.Unchecked_Deallocation;

package body Agpl.Average_queue.Timed is

   ------------------------------------------------------------------------
   -- Extended_Push                                                      --
   ------------------------------------------------------------------------
   --  Gives extra info: If a gap change has happened and how many empty
   --  gaps after it have happened:

   procedure Extended_Push
     (This       : in out Object;
      Data       : in Item;
      Gap_Change : out Boolean;  -- True if at least a new gap has been pushed
      Empty_Gaps : out Natural)  -- Number of empty gaps after the last one
                                 --  pushed
   is
   begin
      This.Safe.Push (Data, Gap_Change, Empty_Gaps);
   end Extended_Push;

   ------------------------------------------------------------------------
   -- Push                                                               --
   ------------------------------------------------------------------------
   procedure Push (This : in out Object; Data : in Item) is
      Gap_Change : Boolean;
      Empty_Gaps : Natural;
   begin
      This.Safe.Push (Data, Gap_Change, Empty_Gaps);
   end Push;

   ------------------------------------------------------------------------
   -- Average                                                            --
   ------------------------------------------------------------------------
   procedure Average (This : in out Object; Result : out Item) is
   begin
      This.Safe.Avg (Result);
   end Average;

   ------------------------------------------------------------------------
   -- Safe_object                                                        --
   ------------------------------------------------------------------------
   protected body Safe_object is
      ----------
      -- Push --
      ----------
      procedure Push
        (Value      : in Item;
         Gap_Change : out Boolean;
         Empty_Gaps : out Natural)
      is
         Now : constant Calendar.Time := Calendar.Clock;
      begin
         if Now - Slot_start > Gap then
            --  Push acum
            Push (Data.all, Acum);
            Gap_Change := True;

            --  Zeroes for elapsed empty gaps
            Empty_Gaps :=
               Natural (Float'Floor
                           (Float ((Now - Slot_start - Gap) / Gap)));
            if Empty_Gaps >= Data.Size then
               for N in  1 .. Data.Size loop
                  Push (Data.all, 0.0);
               end loop;
            else
               for N in  1 .. Empty_Gaps loop
                  Push (Data.all, 0.0);
               end loop;
            end if;

            --  New acum:
            Acum := Value;

            --  New slot_start, the pushed one plus empty ones:
            Slot_start := Slot_start + Gap * (Empty_Gaps + 1);
         else
            Acum       := Acum + Value;
            Gap_Change := False;
            Empty_Gaps := 0;
         end if;
      end Push;

      ---------
      -- Avg --
      ---------
      procedure Avg (Result : out Item) is
         GC : Boolean; -- Out values, not used.
         EG : Natural; -- Out values, not used.
      begin
         Push (0.0, GC, EG); -- Update to current time
         if Is_empty (Data.all) then
            Result := 0.0;
         else
            Result := Average (Data.all) / Item (Gap);
         end if;
      end Avg;

      ------------
      -- Create --
      ------------
      procedure Create is
      begin
         Data := new Average_queue.Object (Size => Slots);
      end Create;

      -------------
      -- Destroy --
      -------------
      procedure Destroy is
         procedure Free is new Unchecked_Deallocation (
            Average_queue.Object'Class,
            Average_queue.Object_access);
      begin
         Free (Data);
      end Destroy;

   end Safe_object;

   procedure Initialize (This : in out Object) is
   begin
      This.Safe.Create;
   end Initialize;

   procedure Finalize (This : in out Object) is
   begin
      This.Safe.Destroy;
   end Finalize;

   ------------------------------------------------------------------------
   -- Free                                                               --
   ------------------------------------------------------------------------
   procedure Free (This : in out Object_Access) is
      procedure Delete is new Ada.Unchecked_Deallocation (
         Object,
         Object_Access);
   begin
      Delete (This);
   end Free;

end Agpl.Average_queue.Timed;
