package Agpl.Protected_Datastore.Integers is

   pragma Preelaborate;

   type Object is new Object_Data with record
      Value : Integer;
   end record;

   type Adder (Operand : Integer) is new Functor with null record;

   procedure Operate (This : in out Adder; Value : in out Object_Data'Class);

end Agpl.protected_Datastore.Integers;
