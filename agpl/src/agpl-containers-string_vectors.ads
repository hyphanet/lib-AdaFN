 

with Ada.Containers.Indefinite_Vectors;

package Agpl.Containers.String_Vectors is new
  Ada.Containers.Indefinite_Vectors (Positive, String);

pragma Preelaborate (Agpl.Containers.String_Vectors);

