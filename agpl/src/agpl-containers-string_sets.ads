 

with Ada.Containers.Indefinite_Ordered_Sets;

package Agpl.Containers.String_Sets is new
  Ada.Containers.Indefinite_Ordered_Sets (String);

pragma Preelaborate (Agpl.Containers.String_Sets);

