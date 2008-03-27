 

--  Protected container for values of any private type.

generic
   type Element_type is private;
package Agpl.Protected_Value is

   pragma Pure;

   type Functor is abstract tagged null record;
   procedure Operate (This : in out Functor; Value : in out Element_Type)
   is abstract;

   protected type Object is
      function  Get return Element_Type;
      procedure Set     (This  : in Element_Type);
      procedure Operate (Using : in out Functor'Class);
      --  Will call Functor.Operate (Value)
   private
      Value : Element_Type;
   end Object;

end Agpl.Protected_Value;
