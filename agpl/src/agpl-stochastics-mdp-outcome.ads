 

--  Used to pack an action with its possible outcomes.

with Agpl.Stochastics.Mdp.Action;
with Agpl.Stochastics.Mdp.State;

package Agpl.Stochastics.Mdp.Outcome is

   pragma Preelaborate;

   type Object is tagged private;

   function Create
     (A : in Action.Object'Class;
      S : in State.Object_Lists.List) return Object;
   --  Receives an action and all the valid reachable states for these.

   function Get_Action (This : in Object) return Action.Object'Class;

   function Get_Action (This : in Object) return Action.Object_Lists.List;
   --  Returns a list containing a single action.

   function Get_States (This : in Object) return State.Object_Lists.List;

private

   type Object is tagged record
      Action : Mdp.Action.Object_Lists.List;
      --  A list with a single element.

      States : State.Object_Lists.List;
      --  States
   end record;

end Agpl.Stochastics.Mdp.Outcome;
