 

--  Root abstract type holding an Action.

with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Agpl.Stochastics.Mdp.Action is

   pragma Preelaborate;

   type Object is abstract tagged null record;

   function To_String (This : in Object) return String is abstract;

   --  Containers to be used elsewhere:
   package Object_Lists is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (Object'Class, "=");


end Agpl.Stochastics.Mdp.Action;
