 

with Agpl.Containers.Integer_Sets;

package Agpl.Containers.Utils is

   pragma preelaborate;

   type Integer_Array is array (Integer range <>) of Integer;

   function To_Set (X : Integer_Array) return Integer_Sets.Set;

end Agpl.Containers.Utils;
