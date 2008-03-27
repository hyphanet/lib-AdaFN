with Agpl.Containers.String_Vectors;

with Ada.Containers.Vectors;

package Agpl.Containers.String_Vector_Vectors is new
Ada.Containers.Vectors (Positive,
                        Agpl.Containers.String_Vectors.Vector,
                        Agpl.Containers.String_Vectors."=");

pragma Preelaborate (Agpl.Containers.String_Vector_Vectors);

