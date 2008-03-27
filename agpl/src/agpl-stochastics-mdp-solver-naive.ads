 

with Agpl.Stochastics.Mdp.Problem;
with Agpl.Stochastics.Mdp.Value_Function;

--  This solver will enumerate reachable states and do a regular
--  value iteration from that point.

package Agpl.Stochastics.Mdp.Solver.Naive is

   pragma Preelaborate;

   procedure Solve
     (Pr : in     Problem.Object;
      E  : in     Evolution_Function;
      T  : in     Transition_Function;
      R  : in     Reward_Function;
      V  : in out Value_Function.Object;
      It : in     Positive := Positive'Last);
   --  It: Maximum iterations to attempt.

end Agpl.Stochastics.Mdp.Solver.Naive;
