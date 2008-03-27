package Adafn.Context is

   pragma Preelaborate;

   type Object is abstract tagged null record;
   --  Used to pass some data to asynchronous transactions.

   type Null_Object_Type is new Object with null record;

   Null_Object : constant Null_Object_Type;

private

   Null_Object : constant Null_Object_Type := (Object with null record);

end Adafn.Context;
