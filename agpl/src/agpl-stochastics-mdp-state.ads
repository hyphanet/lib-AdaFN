 

--  Root abstract type holding a State with the operations that will be needed
--  by the MDP Solver.

with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;

package Agpl.Stochastics.Mdp.State is

   pragma Preelaborate;

   type Object is abstract tagged null record;

   type Object_Id is new String;

   function Distance (This : in Object) return Distances;
   --  Should provide a "distance" to the final state.
   --  Default one gives always 0.0 (wrong). You should redefine it if
   --  appliable for use elsewhere, don't care if not.

   function "<" (L, R : in Object'Class) return Boolean;
   --  Uses the distance and then the Id, to ensure that not < => >.

   function Get_Id (This : in Object) return Object_Id is abstract;
   --  Needed to conveniently store states and reference them.

   function Hash (Id : in Object_Id) return Ada.Containers.Hash_Type;
   pragma Inline (Hash);
   --  Just a wrapper

   function To_String (This : in Object) return String;
   --  For debugging purposes

   --  Containers to be used elsewhere:
   package Object_Lists is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (Object'Class, "=");

   package Object_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps (Object_Id, Object'Class, Hash, "=");

   package Object_Sets is new
     Ada.Containers.Indefinite_Ordered_Sets (Object'Class, "<");

   package Object_Id_Sets is new
     Ada.Containers.Indefinite_Ordered_Sets (Object_Id, "<");

   package Object_Id_Lists is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (Object_Id, "=");

end Agpl.Stochastics.Mdp.State;
