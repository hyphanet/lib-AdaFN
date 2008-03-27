 

package body Agpl.Sequence is

   protected body Object is

      procedure Get_next (Value : out Num) is
      begin
         Value      := Next_value;
         Next_value := Next_value + 1;
      end Get_next;


      function  Peek_next return Num is
      begin
         return Next_Value;
      end Peek_Next;


      procedure Set_Next (Value : in  Num) is
      begin
         Next_Value := Value;
      end Set_Next;

   end Object;

end Agpl.Sequence;
