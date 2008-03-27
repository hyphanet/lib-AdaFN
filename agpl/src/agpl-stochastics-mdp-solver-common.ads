 

with Agpl.Stochastics.Mdp.Containers;
with Agpl.Stochastics.Mdp.Value_Function;

--  Common subprograms useable for all solvers.

package Agpl.Stochastics.Mdp.Solver.Common is

   pragma Preelaborate;

   procedure Appliable_Actions
     (States  : in     State.Object_Lists.List;
      A       : in     Action_Function;
      Actions :    out Containers.State_Actions_Maps.Map);
   --  Gives the actions appliable to some states.

   procedure Appliable_Outcomes
     (Initial  : in     State.Object'Class;
      A        : in     Action_Function;
      AE       : in     Action_Evolution_Function;
      Outcomes :    out Containers.Outcome_Lists.List);
   --  Gives the outcomes for a single state.

   procedure Appliable_Outcomes
     (States   : in     State.Object_Lists.List;
      A        : in     Action_Function;
      AE       : in     Action_Evolution_Function;
      Outcomes :    out Containers.State_Outcomes_Maps.Map);
   --  Gives the outcomes for a series of States.

   procedure Reachable_States
     (Initial : in     State.Object_Lists.List;
      E       : in     Evolution_Function;
      Final   :    out State.Object_Lists.List);
   --  Will compute all states reachable from Initial, at any distance.

   procedure Reachable_With_Policy
     (Initial : in     State.Object_Lists.List;
      AE      : in     Action_Evolution_Function;
      V       : in     Value_Function.Object;
      Final   :    out State.Object_Lists.List);
   --  All states reachable from Initial given some politic, at any distance.

   ---------------
   -- Summaries --
   ---------------

   --  Functions who drop in stdout some info about a problem.

   procedure Summary_State_Action
     (Initial : in     State.Object_Lists.List;
      E       : in     Evolution_Function;
      A       : in     Action_Function);
   --  Prints, for every state, how many actions can be applied to it.

end Agpl.Stochastics.Mdp.Solver.Common;
