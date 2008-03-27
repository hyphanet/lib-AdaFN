 

with System;

generic
   type Num is mod <>;
package Agpl.Sequence is

   pragma Preelaborate;

   protected type Object (
      Initial_value : Num             := Num'First;
      Priority      : System.Priority := System.Priority'Last)
   is
      pragma Priority (Priority);

      procedure Get_next (Value : out Num);
      function  Peek_next return Num;
      procedure Set_next (Value : in  Num);

   private
      Next_value : Num := Initial_value;
   end Object;

end Agpl.Sequence;
