 

package body Agpl.Counter is

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   protected body Object is
      procedure Add (Increment : in Integer := 1) is
      begin
         Value := Value + Increment;
      end Add;

      procedure Reset (Val     : in Integer := 0) is
      begin
         Value := Val;
      end Reset;

      function  Val return Integer is
      begin
         return Value;
      end Val;
   end Object;

end Agpl.Counter;
