with Agpl.Stochastics.Mdp.Outcome;
with Agpl.Trace; use Agpl.Trace;

with Ada.Containers;
with Ada.Exceptions;

package body Agpl.Stochastics.Mdp.Bellman is

   use type State.Object'Class;

   type Prob_Printer is delta 0.000000001 range 0.0 .. 1.0;

   ----------------
   -- Get_Action --
   ----------------

   function Get_Action (This : in Result) return Action.Object'Class is
      use Action.Object_Lists;
      use type Ada.Containers.Count_Type;
   begin
      pragma Assert (Length (This.Action) = 1);

      return Element (First (This.Action));
   end Get_Action;

   ----------------
   -- Get_Reward --
   ----------------

   function Get_Reward (This : in Result) return Rewards is
   begin
      return This.Reward;
   end Get_Reward;

   --------------
   -- Operator --
   --------------

   function Operator
     (Ini : in State.Object'Class;
      Act : in Action.Object_Lists.List;
      Fin : in State.Object_Lists.List;
      V   : in Value_Function.Object;
      T   : in Solver.Transition_Function;
      R   : in Solver.Reward_Function;
      D   : in Discounts)
      return Result
   is
      Res        : Result;
      Total_Prob : Probabilities;
      Total_Rew  : Rewards;
      A          : Action.Object_Lists.Cursor := Action.Object_Lists.First (Act);
      use type Action.Object_Lists.Cursor;
      S          : State.Object_Lists.Cursor  := State.Object_Lists.First (Fin);
      use type State.Object_Lists.Cursor;
   begin
      Res.Reward := Rewards'First;

      Actions :
      while A /= Action.Object_Lists.No_Element loop
         Total_Prob := 0.0; -- For each action
         Total_Rew  := 0.0; -- For each action

         States :
         while S /= State.Object_Lists.No_Element loop
            declare
               Prob : constant Probabilities :=
                        T (Ini,
                           Action.Object_Lists.Element (A),
                           State.Object_Lists.Element (S));

               Rew_A_S    : Rewards;
               Fin_Value  : Rewards;
               use Action.Object_Lists;
            begin
               if Prob > 0.0 then
                  begin
                     Total_Prob := Total_Prob + Prob;
                  exception
                     when Constraint_Error =>
                        Ada.Exceptions.Raise_Exception
                          (Wrong_Data'Identity,
                           "Action: " &
                           Action.To_String (Action.Object_lists.Element (A)) &
                           "; Prob: " & Prob_Printer (Prob)'Img &
                           "; Total: " & Prob_Printer (Total_Prob)'Img);
                  end;

                  Rew_A_S := R (Ini,
                            Action.Object_Lists.Element (A),
                            State.Object_Lists.Element (S));

                  --  Ending state best reward known.
                  --  Use this only if not reflecting to the same state!
                  if State.Object_Lists.Element (S) /= Ini then
                     begin
                        Fin_Value :=
                          Value_Function.Get_Value
                            (V, State.Get_Id (State.Object_Lists.Element (S)));
                     exception
                        when Value_Function.Unknown_Value =>
                           Fin_Value := 0.0;
                     end;
                  else
                     Fin_Value := 0.0;
                  end if;

                  --  Accumulate reward of each outcome times its probability
                  Total_Rew :=
                    Total_Rew +
                    (Rew_A_S + Fin_Value) * Rewards (Prob);
               end if;
            end;

            State.Object_Lists.Next (S);
         end loop States;

         if abs (Total_Prob - 1.0) > Delta_Error then
            Ada.Exceptions.Raise_Exception
              (Wrong_Data'Identity,
               "Action: " & Action.To_String (Action.Object_lists.Element (A)) &
               "; Total_Prob: " & Prob_Printer (Total_Prob)'Img);
         end if;

         --  See if this action is better:
         if Total_Rew > Res.Reward then
            Res.Reward := Total_Rew;
            Res.Action.Clear;
            Res.Action.Append (Action.Object_Lists.Element (A));
         elsif Total_Rew = Res.Reward then
            Log ("[Bellman operator] Ambiguity: Two actions give same reward.",
                 Warning);
         end if;

         S := State.Object_Lists.First (Fin);
         Action.Object_Lists.Next (A);
      end loop Actions;

      --  Apply discount:
      Res.Reward := Res.Reward * Rewards (D);

      return Res;
   end Operator;

   function Pruned_Operator
     (Ini : in State.Object'Class;
      O   : in Containers.Outcome_Lists.List;
      V   : in Value_Function.Object;
      T   : in Solver.Transition_Function;
      R   : in Solver.Reward_Function;
      D   : in Discounts) return Result
   --  We'll use the ordinary bellman operator for each outcome.
   --  We'll keep the best result.
   is
      Res, Local_Res : Result;
      use Containers.Outcome_Lists;
      I   : Cursor := First (O);
   begin
      pragma Assert (Natural (O.Length) > 0);

      Res.Reward := Rewards'First;
      while I /= No_Element loop
         Local_Res := Operator
           (Ini,
            Outcome.Get_Action (Element (I)),
            Outcome.Get_States (Element (I)),
            V,
            T,
            R,
            D);

         if Local_Res.Reward > Res.Reward then
            Res := Local_Res;
         elsif Local_Res.Reward = Res.Reward then
            Log ("[Pruned Bellman operator] Ambiguity: " &
                 "Two actions give same reward.", Warning);
         end if;

         Next (I);
      end loop;

      return Res;
   end Pruned_Operator;

end Agpl.Stochastics.Mdp.Bellman;
