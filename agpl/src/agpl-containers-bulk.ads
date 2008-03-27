

--  Massive instantiation of containers

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Containers.Indefinite_Vectors;

generic
   type Element_Type (<>) is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
   type Index_Type is range <>;
   type Key_Type (<>) is private;
   with function "<" (Left, Right : Key_Type) return Boolean is <>;
   with function Key (Item : Element_Type) return Key_Type is <>;
package Agpl.Containers.Bulk is

   pragma Preelaborate;

   pragma Unreferenced (Key); -- It is used in child packages.

   package Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Element_Type);

   package List_Lists is new ada.containers.doubly_linked_lists
     (Lists.List, Lists."=");

   package Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type, Element_Type);

   package Key_Sets is new Ada.Containers.Indefinite_Ordered_Sets (Key_Type);

   package Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type, Element_Type);

   package String_Element_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps (String, Element_Type);

end Agpl.Containers.Bulk;
