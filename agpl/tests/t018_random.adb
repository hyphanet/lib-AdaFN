with Text_Io; use Text_Io;
with Ada.Exceptions; use Ada.Exceptions;

with Agpl.Random; use Agpl.Random;

procedure T018_Random is
   subtype Test_Range is Integer range 1 .. 2;
   Counter : array (Test_Range) of Natural := (others => 0);
begin
   for I in 1 .. 10000 loop
      declare
         Sample : constant Test_Range := Get_Integer (Test_Range'First,
                                                      Test_Range'Last);
      begin
         Counter (Sample) := Counter (Sample) + 1;
      end;
   end loop;

   for I in Counter'Range loop
      Put_Line (I'Img & ":" & Counter (I)'Img);
   end loop;
exception
   when E : others =>
      Put_Line ("Exception: " & Exception_Information (E));
end T018_Random;
