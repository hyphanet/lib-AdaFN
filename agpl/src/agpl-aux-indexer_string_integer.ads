 

with Ada.Containers.Indefinite_Ordered_Maps;

package Agpl.Aux.Indexer_String_Integer is new
  Ada.Containers.Indefinite_Ordered_Maps (String, Integer, "<", "=");

pragma Preelaborate (Agpl.Aux.Indexer_String_Integer);

