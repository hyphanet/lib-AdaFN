 

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

package Agpl.Containers.String_String_Maps is
new Ada.Containers.Indefinite_Hashed_Maps
  (String, String, Ada.Strings.Hash, "=");

pragma Preelaborate (Agpl.Containers.String_String_Maps);
