 

--  Root package for MDP solvers.

with Agpl.Stochastics.Mdp.Action;
with Agpl.Stochastics.Mdp.State;

package Agpl.Stochastics.Mdp.Solver is

   pragma Preelaborate;

   type Action_Function is access function
     (Initial : in State.Object'Class)
      return       Action.Object_Lists.List;
   --  This function must return all appliable actions to some state.

   type Action_Evolution_Function is access function
     (Initial : in State.Object'Class;
      Action  : in Mdp.Action.Object'Class)
      return       State.Object_Lists.List;
   --  All states reachable from a single state, given an action.

   type Evolution_Function is access function
     (Initial : in State.Object'Class)
      return       State.Object_Lists.List;
   --  This function must return all reachable states from another given one.

   type Involution_Function is access function
     (Final : in State.Object'Class)
      return     State.Object_Lists.List;
   --  Gives states from where the Final state is reachable.

   type Reward_Function is access function
     (Initial : in State.Object'Class;
      Doing   : in Action.Object'Class;
      Final   : in State.Object'Class)
      return       Rewards;
   --  This function is the typical reward function for MDPs

   type Transition_Function is access function
     (Initial : in State.Object'Class;
      Doing   : in Action.Object'Class;
      Final   : in State.Object'Class)
      return       Probabilities;
   --  This function is the typical transition function for MDPs

end Agpl.Stochastics.Mdp.Solver;
