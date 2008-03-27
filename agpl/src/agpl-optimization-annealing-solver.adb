 

with Agpl.Chronos;
with Agpl.Conversions; use Agpl.Conversions;
with Agpl.Trace; use Agpl.Trace;

package body Agpl.Optimization.Annealing.Solver is

   --------------
   -- Add_Move --
   --------------

   procedure Add_Move (This     : in out Object;
                       Move     : in     String;
                       Accepted : in     Boolean)
   is
      use Stat_Maps;
      I : Cursor := This.Stats.Find (Move);

      procedure Do_It (Key : in String; Move : in out Move_Stats) is
         pragma Unreferenced (Key);
      begin
         Move.Taken := Move.Taken + 1;
         if Accepted then
            Move.Accepted := Move.Accepted + 1;
         end if;
      end Do_It;

      Ok : Boolean;
   begin
      if not Has_Element (I) then
         This.Stats.Insert (Move, (others => <>), I, Ok);
         pragma Assert (Ok);
      end if;

      This.Stats.Update_Element (I, Do_It'Access);
   end Add_Move;

   ---------------
   -- Best_Cost --
   ---------------

   function Best_Cost (This : in Object) return Cost is
   begin
      return This.Best_Cost;
   end Best_Cost;

   -------------------
   -- Best_Solution --
   -------------------

   function Best_Solution (This : in Object) return Solution is
   begin
      return This.Best_Sol.Get;
   end Best_Solution;

   ------------------
   -- Current_Cost --
   ------------------

   function Current_Cost (This : in Object) return Cost is
   begin
      return This.Curr_Cost;
   end Current_Cost;

   ----------------------
   -- Current_Solution --
   ----------------------

   function Current_Solution (This : in Object) return Solution is
   begin
      return This.Curr_Sol.Get;
   end Current_Solution;

   -------------------------
   -- Current_Temperature --
   -------------------------

   function Current_Temperature (This : in Object) return Temperature is
   begin
      return This.Curr_Temp;
   end Current_Temperature;

   -------------
   -- Iterate --
   -------------

   procedure Iterate (This   : in out Object;
                      Anneal : not null access function
                        (T : in Temperature) return Temperature)
   is
   begin
      declare
         New_Cost :          Cost;
         P        : constant Float := Random (This.Random_Gen);
         Goodness :          Float;
      begin
         Mutate (This.Curr_Sol.Ref.all); -- Mutate in place
         New_Cost := Evaluate (This.Curr_Sol.Ref.all);
         Goodness := Float (Normalize
                              (Old_Cost => This.Curr_Cost,
                               New_Cost => New_Cost,
                               Temp     => This.Curr_Temp));

         --         Log ("Move: " & Last_Mutation (New_Sol), Always);
         This.Iterations := This.Iterations + 1;

         Log ("[NC/OC/Rnd/Goodnes]:" &
              Image (New_Cost) & "/" &
              Image (This.Curr_Cost) & "/" &
              Image (Cost (P)) & "/" &
              Image (Cost (Goodness)),
              Debug, Detail_Section);

         if New_Cost = Infinite then -- Invalid solution
            This.Wasted := This.Wasted + 1;
            This.Add_Move
              (Last_Mutation (This.Curr_Sol.Ref.all), Accepted => False);
            Undo (This.Curr_Sol.Ref.all);
         elsif New_Cost < This.Best_Cost or else P < Goodness then
            --  See if we must replace the current solution
            --              Log ("New Sol accepted with " &
            --                   Strings.To_String (P) & " < " &
            --                   Strings.To_String (Goodness), Always);

            --  Keep best solution seen:
            if New_Cost < This.Best_Cost then
               This.Best_Cost := New_Cost;
               Log ("Solver: Replacing best solution.", Debug, Log_Section);
               This.Best_Sol.Set (This.Curr_Sol.Get);
            end if;

            This.Curr_Cost := New_Cost;

            This.Add_Move
              (Last_Mutation (This.Curr_Sol.Ref.all), Accepted => True);
         else
            This.Discarded := This.Discarded + 1;
            Undo (This.Curr_Sol.Ref.all);
            This.Add_Move
              (Last_Mutation (This.Curr_Sol.Ref.all), Accepted => False);
         end if;

         This.Curr_Temp := Anneal (This.Curr_Temp);
      end;
   exception
      when E : others =>
         Log ("Annealing.Solver.Iterate: " & Report (E), Error);
         raise;
   end Iterate;

   -----------------
   -- Print_Stats --
   -----------------

   procedure Print_Stats (Stats : in Stat_Maps.Map) is
      use Stat_Maps;

      Total_Moves : Natural := 0;
      Total_Good  : Natural := 0;

      procedure Do_Count (I : Cursor) is
      begin
         Total_Moves := Total_Moves + Element (I).Taken;
         Total_Good  := Total_Good  + Element (I).Accepted;
      end Do_Count;

      procedure Do_Inform (I : Cursor) is
         M : constant Move_Stats := Element (I);
      begin
         Log ("Mutation [" & Key (I) & "] accept/total: " &
              To_String (M.Accepted) & "/" & To_String (M.Taken) & " (" &
              To_String (Float (M.Accepted) * 100.0 / Float (M.Taken)) &
              "%); global %: " &
              To_String (Float (M.Accepted) * 100.0 / Float (Total_Good)) &
              "%/" &
              To_String (Float (M.Taken) * 100.0 / Float (Total_Moves)) & "%",
              Informative, Log_Section);
      end Do_Inform;
   begin
      Stats.Iterate (Do_Count'Access);

      Log ("", Informative, Log_Section);

      Stats.Iterate (Do_Inform'Access);

      Log ("", Informative, Log_Section);

      Log ("TOTAL MOVES (accept/total): " &
           To_String (Total_Good) & "/" & To_String (Total_Moves) & " (" &
           To_String (Float (Total_Good) * 100.0 / Float (Total_Moves)) &
           "%)",
           Informative, Log_Section);

      Log ("", Informative, Log_Section);
   end Print_Stats;

   -----------------
   -- Print_Stats --
   -----------------

   procedure Print_Stats (This : in Object) is
   begin
      Print_Stats (This.Stats);
   end Print_Stats;

   -----------------
   -- Reset_Stats --
   -----------------

   procedure Reset_Stats (This : in out Object) is
   begin
      This.Stats.Clear;
   end Reset_Stats;

   --------------------------
   -- Set_Initial_Solution --
   --------------------------

   procedure Set_Initial_Solution (This : in out Object;
                                   Sol  : in     Solution)
   is
   begin
      This.Best_Sol.Set (Sol);
      This.Curr_Sol.Set (Sol);

      This.Best_Cost := Evaluate (Sol);
      This.Curr_Cost := This.Best_Cost;

      This.Curr_Temp := Temperature'Last;

      This.Discarded  := 0;
      This.Iterations := 0;
      This.Wasted     := 0;

      Log ("Annealing: Setting initial solution with cost " &
           To_String (Float (This.Best_Cost)), Debug, Section => Log_Section);
   end Set_Initial_Solution;

   -----------------------
   -- Set_Best_Solution --
   -----------------------

   procedure Set_Best_Solution (This : in out Object;
                                Sol  : in     Solution)
   is
   begin
      This.Best_Sol.Set (Sol);
      This.Best_Cost := Evaluate (Sol);
   end Set_Best_Solution;

   --------------------------
   -- Set_Current_Solution --
   --------------------------

   procedure Set_Current_Solution (This : in out Object;
                                   Sol  : in     Solution)
   is
   begin
      This.Curr_Sol.Set (Sol);
      This.Curr_Cost := Evaluate (Sol);
   end Set_Current_Solution;

   -----------
   -- Solve --
   -----------

   procedure Solve (This       : in out Object;
                    Ini_Sol    : in     Solution;
                    Anneal     : not null access function
                      (T : in Temperature) return Temperature;
                    Iterations : in     Positive;
                    Timeout    : in     Duration;
                    Converge   : in     Duration;
                    Progress   : access procedure (Continue : out Boolean) := null)
   is
      use Chronos;
      Global_Timer,
      Converge_Timer : Chronos.Object;
      --  Info_Timer     : Chronos.Object;

      Best_Cost      : Cost;
--        Remaining_Iter : Natural := Iterations;
   begin
      This.Set_Initial_Solution (Ini_Sol);
      Best_Cost := This.Best_Cost;

      Log ("(Startup) Initial solution cost is " &
           To_String (Float (Best_Cost)), Debug,
           Section => Log_Section);

      Work (This, Anneal, Iterations, Timeout, Converge, Progress);

      Print_Stats (This.Stats);

      Log ("Best cost found: " & To_String (Float (Best_Cost)) &
           " in" & This.Iterations'Img & " iterations run (" &
           Image (Global_Timer) & ", " &
           To_String
             (Float (This.Iterations) / Float (Elapsed (Global_Timer))) &
           " i/s) (" &
           Integer'Image (This.Wasted * 100 / This.Iterations) & "% wasted moves)" &
           " (" & Integer'Image (This.Discarded * 100 / This.Iterations) & "% discarded moves)",
           Debug, Section => Log_Section);

--        if Remaining_Iter = 0 then
--           Log ("Annealing cycles exhausted", Debug, Section => Log_Section);
      if Elapsed (Converge_Timer) >= Converge then
         Log ("No progress found in convergence period",
              Debug, Section => Log_Section);
      elsif Elapsed (Global_Timer) >= Timeout then
         Log ("Annealing time exhausted", Debug, Section => Log_Section);
      else
         Log ("Iterations exahusted or convergence found", Debug, Section => Log_Section);
      end if;

   end Solve;

   ----------
   -- Work --
   ----------

   procedure Work (This                     : in out Object;
                   Anneal                   : not null access function
                     (T : in Temperature) return Temperature;
                   Iterations               : in     Positive;
                   Timeout                  : in     Duration;
                   Converge                 : in     Duration;
                   Progress                 : access procedure
                     (Continue : out Boolean) := null;
                   Inform_At_End            : in     Boolean := False)
   is
      use Chronos;
      Global_Timer,
      Converge_Timer : Chronos.Object;
--      Info_Timer     : Chronos.Object;

      Best_Cost      : Cost;
      Remaining_Iter : Natural := Iterations;
      Continue       : Boolean := True;
   begin
      while Continue and then
            Remaining_Iter > 0 and then
            Elapsed (Converge_Timer) < Converge and then
            Elapsed (Global_Timer) < Timeout
      loop
         Best_Cost := This.Best_Cost;

         This.Iterate (Anneal);

         Log ("Iteration:" & This.Iterations'Img,
              Debug, Section => Detail_Section);

         --  Check for progress...
         declare
            Curr_Best : constant Cost := This.Best_Cost;
         begin
            if Curr_Best < Best_Cost and then Elapsed (Global_Timer) > 0.0001 then
               Best_Cost := Curr_Best;
               Converge_Timer.Reset;
               begin
                  Log ("(Iteration" & This.Iterations'Img &
                    " at " & Image (Global_Timer) & ") " &
                    "(speed: " & To_String
                      (Float (This.Iterations) /
                         Float (Elapsed (Global_Timer))) &
                    " i/s)" & " Best solution: " &
                    To_String (Float (Best_Cost)) &
                    " obtained via " & Last_Mutation (This.Best_Solution),
                    Debug,
                    Section => Log_Section);
               exception
                  when E : others =>
                     Log ("Solver [report]: " & Report (E),
                          Warning, Log_Section);
                     Log ("Solver [report]: This is probably safe to ignore (?)",
                          Warning, Log_Section);
               end;
            end if;
         end;

         Remaining_Iter := Remaining_Iter - 1;

         if Progress /= null then
            Progress.all (Continue);
         end if;

      end loop;

      if Inform_At_End then
         Print_Stats (This.Stats);

         Log ("Best cost found: " & To_String (Float (Best_Cost)) &
              " in" & This.Iterations'Img & " iterations run (" &
              Image (Global_Timer) & ", " &
              To_String
                (Float (This.Iterations) / Float (Elapsed (Global_Timer))) &
              " i/s) (" &
              Integer'Image (This.Wasted * 100 / This.Iterations) & "% wasted moves)" &
              " (" & Integer'Image (This.Discarded * 100 / This.Iterations) & "% discarded moves)",
              Debug, Section => Log_Section);
      end if;
   end Work;

end Agpl.Optimization.Annealing.Solver;
