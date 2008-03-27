 

--  Protected container for values of any private type.

with Agpl.Protected_Value;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

package Agpl.Protected_Values.Ustrings is
new Agpl.Protected_Value (Ustring);

pragma preelaborate (Agpl.Protected_Values.Ustrings);
