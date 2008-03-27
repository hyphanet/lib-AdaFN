with Text_Io; use Text_Io;
with Ada.Exceptions; use Ada.Exceptions;

with Agpl.Optimization.Mtsp;
use  Agpl.Optimization.Mtsp;

procedure T006_mtsp is
   CITIES : constant := 10;
   SALESMEN : constant := 1;
   Costs  : Cost_Matrix :=
              (1 .. SALESMEN => (1 .. CITIES => (1 .. CITIES => 1.0)));
   Start  : Start_Matrix (Costs'Range (1));
begin
   --  Prepare Start
   for I in Start'Range loop Start (I) := I; end loop;

   --  Alter some costs
   Costs (1, 1, 4) := 0.1;
--   Costs (2, 2, 6) := 0.1;
--   Costs (3, 3, 7) := 0.1;

   declare
      Res : constant Result_Matrix := Brute_Force (Costs);
   begin
      Put_Line ("Solution found. Total  Cost: " & Get_Total_Cost (Costs, Res)'Img);
      Put_Line ("Solution found. MinMax Cost: " & Get_Max_Min_Cost (Costs, Res)'Img);
      for S in Res'Range (1) loop
         Put ("Salesman" & S'Img & ":");
         for C in Res'Range (2) loop
            Put (Res (S, C)'Img);
         end loop;
         New_Line;
      end loop;
   end;
exception
   when E : others =>
      Put_Line ("Exception: " & Exception_Information (E));
end T006_mtsp;
