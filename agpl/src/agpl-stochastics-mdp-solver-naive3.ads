 

with Agpl.Stochastics.Mdp.Problem;
with Agpl.Stochastics.Mdp.Value_Function;

--  This solver will enumerate reachable states.
--  Will too enumerate appliable actions to a state and use only these.
--  Will too enumerate reachable states from an action and use only these.
--  Value iteration from that point.

package Agpl.Stochastics.Mdp.Solver.Naive3 is

   pragma Preelaborate;

   procedure Solve
     (Pr : in     Problem.Object;
      E  : in     Evolution_Function;
      T  : in     Transition_Function;
      R  : in     Reward_Function;
      A  : in     Action_Function;
      AE : in     Action_Evolution_Function;
      V  : in out Value_Function.Object;
      It : in     Positive := Positive'Last);
   --  It: Maximum iterations to attempt.

end Agpl.Stochastics.Mdp.Solver.Naive3;
