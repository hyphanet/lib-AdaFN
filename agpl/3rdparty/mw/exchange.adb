--  GENERIC EXCHANGE PROCEDURE
   --------------------------

--  Creation : 17-NOV-1989 by Mats Weber.


procedure Exchange (X, Y : in out Item) is

   Temp : constant Item := X;

begin
   X := Y;
   Y := Temp;
end Exchange;
