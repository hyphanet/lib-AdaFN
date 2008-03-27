 

--  Root abstract type holding a State with the operations that will be needed
--  by the MDP Solver.

with Agpl.Stochastics.Mdp.Action;
with Agpl.Stochastics.Mdp.State;

package Agpl.Stochastics.Mdp.Problem is

   pragma Preelaborate;

   type Object is private;

   procedure Create
     (This            :    out Object;
      Initial_States  : in     State.Object_Lists.List;
      Final_States    : in     State.Object_Lists.List;
      Actions         : in     Action.Object_Lists.List;
      Discount        : in     Discounts := 0.95);
   --  Creates the basic definition of a problem

   function Get_Actions
     (This : in Object)
      return Action.Object_Lists.List;
   --  Obtain the actions applying to this MDP.

   function Get_Discount (This : in Object) return Discounts;

   function Get_Final_States (This : in Object)
                              return State.Object_Lists.List;
   --  Obtain desired final states (if apply).

   function Get_Initial_States (This : in Object)
                                return State.Object_Lists.List;
   --  Obtain the initial states for the problem

private

   type Object is record
      Initial_States : State.Object_Lists.List;
      Final_States   : State.Object_Lists.List;
      Actions        : Action.Object_Lists.List;
      Discount       : Discounts;
   end record;

end Agpl.Stochastics.Mdp.Problem;
