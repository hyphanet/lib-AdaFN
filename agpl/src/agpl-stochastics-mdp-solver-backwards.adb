with Agpl.Stochastics.Mdp.Bellman;
with Agpl.Stochastics.Mdp.Containers;
with Agpl.Stochastics.Mdp.Outcome;
with Agpl.Stochastics.Mdp.Solver.Common;
with Agpl.Stochastics.Mdp.State;
with Agpl.Trace; use Agpl.Trace;

package body Agpl.Stochastics.Mdp.Solver.Backwards is

   package OL  renames Containers.Outcome_Lists;
   package SL  renames State.Object_Lists;
--   package SM  renames State.Object_Maps;
   package SS  renames State.Object_Sets;
   package SIS renames State.Object_Id_Sets;

   use type OL.Cursor;
   use type SL.Cursor;
   use type State.Object'Class;

   Debug : constant Boolean := False;

   -----------
   -- Solve --
   -----------

   procedure Solve
     (Pr  : in     Problem.Object;
      I   : in     Involution_Function;
      A   : in     Action_Function;
      AE  : in     Action_Evolution_Function;
      T   : in     Transition_Function;
      R   : in     Reward_Function;
      V   : in out Value_Function.Object;
      It  : in     Positive := Positive'Last)
   is

      ------------
      -- Append --
      ------------

      procedure Append (This : in out SS.Set; S : in State.Object'Class) is
         Pos : SS.Cursor;
         Ok  : Boolean;
      begin
         SS.Insert (This, S, Pos, Ok);
         --  pragma Assert (Ok);
         --  Could be already there from some other node expansion.
      end Append;

      procedure Append (This : in out SIS.Set; S : in State.Object'Class) is
         Pos : SIS.Cursor;
         Ok  : Boolean;
      begin
         SIS.Insert (This, State.Get_Id (S), Pos, Ok);
         pragma Assert (Ok);
      end Append;

      ------------
      -- To_Set --
      ------------

      function To_Set (This : in SL.List) return SS.Set is
         S   : SS.Set;
         I   : SL.Cursor := SL.First (This);
         Pos : SS.Cursor;
         Ok  : Boolean;
      begin
         while I /= SL.No_Element loop
            S.Insert (SL.Element (I), Pos, Ok);
            pragma Assert (Ok);
            SL.Next (I);
         end loop;
         return S;
      end To_Set;

      Next_States      : SS.Set;
      --  States discovered in this iteration to be attempted in the next one.

      Pending_States   : SS.Set := To_Set (Problem.Get_Final_States (Pr));
      --  States to be processed for the current iteration

      Expanded         : SIS.Set;
      --  Ids of States already expanded.

      Iterations       : Natural := 0;
   begin
      if Debug then
         Log ("** Warning ** Debug mode is active", Always);
      end if;

      Main :
      loop
         Log ("Iteration" & Natural'Image (Iterations + 1),
              Trace.Debug, Log_Section);

         Loop_Pending_States :
         while not Pending_States.Is_Empty loop
            Pending_State_Checking :
            declare
               FiS  : constant State.Object'Class :=
                        Pending_States.First_Element;
               --  Final State

               Prev : SS.Set := To_Set (I (FiS));
               --  The precedent states in closer first order.
            begin
               if Verbose then
                  Log (" Expanding " & State.To_String (Fis) & " with distance" &
                       Duration'Image (Duration (Fis.Distance)),
                       Trace.Debug, Log_Section);
               end if;

               Pending_States.Delete_First; -- Already used.

               Append (Expanded, FiS);

               Initial_States :
               while not Prev.Is_Empty loop
                  Initial_State_Checking :
                  declare
                     InS : constant State.Object'Class := Prev.First_Element;
                  begin
                     Prev.Delete_First; -- Already used

                     if Verbose then
                        Log ("   Initial state: " & State.To_String (Ins) &
                             " with distance" & Duration'Image
                               (Duration (Ins.Distance)),
                             Trace.Debug, Log_Section);
                     end if;

                     --  Insert if needed the expanded state in Next_States.
                     if False then
                        null;
                     elsif SS.Contains (Pending_States, InS) then
                        if Verbose then
                           Log ("      (Already pending)", Always);
                        end if;
                     elsif SIS.Contains (Expanded, State.Get_Id (InS)) then
                        if Verbose then
                           Log ("      (Already expanded)", Always);
                        end if;
                     else
                        Append (Next_States, InS);
                        if Verbose then
                           Log ("      (Queued)", Always);
                        end if;
                     end if;

                     --  By the problem definition, if a state has been already
                     --  valued, there's no need to examine it again:
                     if Debug or else not Value_Function.Contains (V, InS) then
                        --  Assert that all the final states have been already
                        --  valued (except for itself).
                        Outcomes_Checking :
                        declare
                           Outc : OL.List;
                           I    : OL.Cursor;
                           Best : Bellman.Result;
                        begin
                           Common.Appliable_Outcomes (InS, A, AE, Outc);
                           I := OL.First (Outc);

                           if Debug then
                           while I /= OL.No_Element loop
                              declare
                                 Succ : constant SL.List := Outcome.Get_States
                                   (OL.Element (I));
                                 J    : SL.Cursor := SL.First (Succ);
                              begin
                                 while J /= SL.No_Element loop
                                    pragma Assert
                                      (SL.Element (J) = InS or else
                                       Value_Function.Contains (V, SL.Element (J)));
                                    SL.Next (J);
                                 end loop;
                              end;
                              OL.Next (I);
                              end loop;
                           end if;

                           --  Do Pruned Bellman to see the best action for this
                           --  initial state.
                           Best := Bellman.Pruned_Operator
                             (Ins, Outc, V, T, R, Problem.Get_Discount (Pr));

                           --  Compare, since the InS may appear expanded from
                           --  several states already reached.
                           Reward_Checking :
                           declare
                              Val : Rewards := Rewards'First;
                           begin
                              if Value_Function.Contains (V, InS) then
                                 Val := Value_Function.Get_Value (V, InS);
                                 pragma Assert
                                   (Val = Bellman.Get_Reward (Best));
                              end if;

                              if
                                Bellman.Get_Reward (Best) > Val or else
                                not Value_Function.Contains (V, InS)
                              then
                                 Val := Bellman.Get_Reward (Best);
                                 Value_Function.Set_Value
                                   (V, InS,
                                    Val,
                                    Bellman.Get_Action (Best));
                              end if;
                           end Reward_Checking;
                        end Outcomes_Checking;

                        if Verbose then
                           Log ("      Best action found: " &
                             Action.To_String
                               (Value_Function.Get_Action (V, Ins)) &
                             " [" & Duration'Image
                               (Duration
                                  (Value_Function.Get_Value (V, Ins))) &
                             " ]", Always);
                        end if;

                     end if;

                  end Initial_State_Checking;
               end loop Initial_States;
            end Pending_State_Checking;
         end loop Loop_Pending_States;

         --  Exit if no new states discovered
         exit Main when Next_States.Is_Empty;

         --  Check all initials reached for due exit
         declare
            Ini  : constant SL.List := Problem.Get_Initial_States (Pr);
            I    : SL.Cursor        := SL.First (Ini);
            Fail : Boolean          := False;
         begin
            while I /= SL.No_Element and then not Fail loop
               if not Value_Function.Contains (V, SL.Element (I)) then
                  Fail := True;
               end if;
               SL.Next (I);
            end loop;

            if not Fail then
               Log ("Solution found in" &
                    Natural'Image (Iterations + 1) &" iterations", Informative);
               Log ("States expanded:" &
                    Natural'Image (Natural (Sis.Length (Expanded))),
                    Informative);
               exit Main; -- <-- Exit due to resolution
            end if;
         end;

         --  Verify backteracy counter for early exit
         Iterations := Iterations + 1;
         if Iterations = It then
            Log ("No solution found in" &
                 Natural'Image (Iterations + 1) & " iterations", Warning);
            Log ("States expanded:" &
                 Natural'Image (Natural (Sis.Length (Expanded))), Warning);
            exit Main;
         end if;

         --  Move all in Next to Pending
         Pending_States := Next_States;
         Next_States.Clear;
      end loop Main;
   end Solve;

end Agpl.Stochastics.Mdp.Solver.Backwards;
