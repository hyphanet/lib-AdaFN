 

with Ada.Containers.Indefinite_Ordered_Maps;

package Agpl.Containers.String_Float_Maps is
new Ada.Containers.Indefinite_Ordered_Maps (String, Float);

pragma Preelaborate (Agpl.Containers.String_Float_Maps);
