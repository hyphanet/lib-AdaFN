 

--  Protected container for values of any private type.

package body Agpl.Protected_Value is

   protected body Object is
      function  Get return Element_Type is
      begin
         return Value;
      end Get;
      procedure Set (This : in Element_Type) is
      begin
         Value := This;
      end Set;
      procedure Operate (Using : in out Functor'Class) is
      begin
         --  Dispatching call
         Operate (Using, Value);
      end Operate;
   end Object;

end Agpl.Protected_Value;
