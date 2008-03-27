 

--  Protected container for values of any private type.

with Agpl.Protected_Value;

package Agpl.Protected_Values.Duration is
  new Agpl.Protected_Value (Duration);

pragma Preelaborate (Agpl.Protected_Values.Duration);
