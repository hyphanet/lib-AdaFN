 

with Agpl.Stochastics.Mdp.Problem;
with Agpl.Stochastics.Mdp.Value_Function;

--  This solver is based in the principle that theres a D (S, F), a distance
--  function between any state S and the final state F, which verifies:

--    for every S' / T (S, a, S') > 0 => D (S', F) <= D (S, F)

--  In essence, any factible transition goes towards the goal.

--  Furthermore, is necessary that, from any state, the goal state be reachable.

--  The involution and evolution functions must be that the following is true:
--     Any precedent state, when evoluted, leads to states already generated
--     be the involution function (i.e: simmetry must hold. No missing states
--     in the way back).

--  Backtracing from the Final state, all potential states are traced back.
--  Bellman operator is applied in the same process.
--  Here the Iteration refers to back-steps to take since no convergence occurs.

--  If a initial state is provided, once it is reached, the search stops.

--  Some caveats: discount has no meaning here. Thus only MDPs with final
--  states which are absorbent should be resolved.
--  By the same reason, there shouldn't be local optima.
--  The final state must be reachable in arbitrary steps from any other state.
--  If not, the solution will be incomplete. Or something worse. :$

package Agpl.Stochastics.Mdp.Solver.Backwards is

   pragma Preelaborate;

   procedure Solve
     (Pr  : in     Problem.Object;
      I   : in     Involution_Function;
      A   : in     Action_Function;
      AE  : in     Action_Evolution_Function;
      T   : in     Transition_Function;
      R   : in     Reward_Function;
      V   : in out Value_Function.Object;
      It  : in     Positive := Positive'Last);
   --  It: Maximum backsteps to attempt.
   --  The State.Distance function must have been properly overloaded.

end Agpl.Stochastics.Mdp.Solver.Backwards;
