 

--  Protected container for values of any private type.

with Agpl.Protected_Value;

package Agpl.Protected_Values.Integers is

   pragma Preelaborate;

   package Int is new Agpl.Protected_Value (Integer);

   type Adder (Operand : Integer) is
      new Int.Functor with null record;
   procedure Operate (This : in out Adder; Value : in out Integer);


end Agpl.Protected_Values.Integers;
