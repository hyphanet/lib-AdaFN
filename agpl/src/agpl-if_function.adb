function Agpl.If_Function
  (Condition : in Boolean;
   If_True   : in Value;
   If_False  : in Value)
   return Value
is
begin
   if Condition then
      return If_True;
   else
      return If_False;
   end if;
end Agpl.If_Function;

