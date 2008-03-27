 

--  Root package for MDP solvers.

with Agpl.Stochastics.Mdp.Action;
with Agpl.Stochastics.Mdp.Containers;
with Agpl.Stochastics.Mdp.Solver;
with Agpl.Stochastics.Mdp.State;
with Agpl.Stochastics.Mdp.Value_Function;

package Agpl.Stochastics.Mdp.Bellman is

   pragma Preelaborate;

   Wrong_Data : exception;
   --  Raised when the action probabilities don't add to 1.0

   type Result is private;
   --  This opaque type is used to return a Reward + Action pair.
   --  It's necessary because the Action is unconstrained.

   function Get_Action (This : in Result) return Action.Object'Class;
   pragma Inline (Get_Action);

   function Get_Reward (This : in Result) return Rewards;
   pragma Inline (Get_Reward);

   function Operator
     (Ini : in State.Object'Class;
      Act : in Action.Object_Lists.List;
      Fin : in State.Object_Lists.List;
      V   : in Value_Function.Object;
      T   : in Solver.Transition_Function;
      R   : in Solver.Reward_Function;
      D   : in Discounts) return Result;
   --  Applies the Bellman Operator, obtaining the new best Reward for a state.

   function Pruned_Operator
     (Ini : in State.Object'Class;
      O   : in Containers.Outcome_Lists.List;
      V   : in Value_Function.Object;
      T   : in Solver.Transition_Function;
      R   : in Solver.Reward_Function;
      D   : in Discounts) return Result;
   --  Applies the Bellman Operator, obtaining the new best Reward for a state.
   --  Uses an already pruned list of Outcomes for the State.

private

   type Result is record
      Reward : Rewards;
      Action : Mdp.Action.Object_Lists.List;
      --  A list containing a single action.
   end record;

end Agpl.Stochastics.Mdp.Bellman;
