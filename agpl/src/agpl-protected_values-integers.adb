 

--  Protected container for values of any private type.

package body Agpl.Protected_Values.Integers is

   procedure Operate (This : in out Adder; Value : in out Integer) is
   begin
      Value := Value + This.Operand;
   end Operate;

end Agpl.Protected_Values.Integers;
