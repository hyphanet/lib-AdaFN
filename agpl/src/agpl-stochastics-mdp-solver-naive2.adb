with Agpl.Stochastics.Mdp.Bellman;
with Agpl.Stochastics.Mdp.Containers;
with Agpl.Stochastics.Mdp.Solver.Common;
with Agpl.Trace; use Agpl.Trace;

package body Agpl.Stochastics.Mdp.Solver.Naive2 is

   package CSA renames Containers.State_Actions_Maps;

   -----------
   -- Solve --
   -----------

   procedure Solve
     (Pr : in     Problem.Object;
      E  : in     Evolution_Function;
      T  : in     Transition_Function;
      R  : in     Reward_Function;
      A  : in     Action_Function;
      V  : in out Value_Function.Object;
      It : in     Positive := Positive'Last)
   is
      Reachable  : State.Object_Lists.List;
      Actions    : CSA.Map;
      Iterations : Natural := 0;
   begin
      --  Obtain reachable states
      Common.Reachable_States (Problem.Get_Initial_States (Pr), E, Reachable);
      Log ("# Reachable states:" & Natural'Image
           (Natural (State.Object_Lists.Length (Reachable))),
           Debug, Log_Section);

      --  Obtain actions for each state
      Common.Appliable_Actions (Reachable, A, Actions);
      Log ("Enumerated appliable actions", Debug, Log_Section);

      --  Perform value iteration over reachable states
      loop
         declare
            use State.Object_Lists;
            S : State.Object_Lists.Cursor := State.Object_Lists.First (Reachable);

            Vi : Value_Function.Object;
            use type Value_Function.Object;

            Result : Bellman.Result;
         begin
            while S /= No_Element loop
               Result := Bellman.Operator
                 (Element (S),
                  CSA.Element (CSA.Find (Actions, State.Get_Id (Element (S)))),
                  Reachable,
                  V,
                  T,
                  R,
                  Problem.Get_Discount (Pr));

               Value_Function.Set_Value
                 (Vi,
                  State.Object_Lists.Element (S),
                  Bellman.Get_Reward (Result),
                  Bellman.Get_Action (Result));

               Next (S);
            end loop;

            if Verbose then
               Value_Function.Summary (Vi);
            end if;

            Iterations := Iterations + 1;
            exit when Vi = V;
            V := Vi;
         end;

         exit when Iterations = It;
         Log ("Iteration" & Iterations'Img, Debug, Log_Section);

      end loop;

      if Iterations = It then
         Log ("Convergence not reached after" & It'Img & " iterations",
              Warning);
      else
         Log ("Converged in" & Iterations'Img & " iterations", Informative);
      end if;

   end Solve;

end Agpl.Stochastics.Mdp.Solver.Naive2;
