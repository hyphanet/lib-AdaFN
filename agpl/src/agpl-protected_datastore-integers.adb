package body Agpl.Protected_Datastore.Integers is

   -------------
   -- Operate --
   -------------

   procedure Operate
     (This : in out Adder;
      Value : in out Object_Data'Class)
   is
      I : Object renames Object (Value);
   begin
      I.Value := I.Value + This.Operand;
   end Operate;

end Agpl.Protected_Datastore.Integers;
