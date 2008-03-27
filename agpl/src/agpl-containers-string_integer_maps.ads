 

with Ada.Containers.Indefinite_Ordered_Maps;

package Agpl.Containers.String_Integer_Maps is
new Ada.Containers.Indefinite_Ordered_Maps (String, Integer);

pragma Preelaborate (Agpl.Containers.String_Integer_Maps);
