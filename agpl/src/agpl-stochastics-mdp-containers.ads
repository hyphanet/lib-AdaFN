 

--  Containers used elsewhere in the MDP packages.

with Agpl.Stochastics.Mdp.Action;
with Agpl.Stochastics.Mdp.Outcome;
with Agpl.Stochastics.Mdp.State;

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;

package Agpl.Stochastics.Mdp.Containers is

   pragma Preelaborate;

   package State_Actions_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (State.Object_Id,
      Action.Object_Lists.List,
      State.Hash,
      State."=",
      Action.Object_Lists."=");

   package Outcome_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Outcome.Object'Class, Outcome."=");

   package State_Outcomes_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (State.Object_Id,
      Outcome_Lists.List,
      State.Hash,
      State."=",
      Outcome_Lists."=");

end Agpl.Stochastics.Mdp.Containers;
