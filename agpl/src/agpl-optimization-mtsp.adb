 

with Permutations;

with Agpl.Text_IO; use Agpl.Text_IO;

package body Agpl.Optimization.Mtsp is

   --------------------
   -- Print_Solution --
   --------------------

   procedure Print_Solution (Costs : in Cost_Matrix; Res : in Result_Matrix) is
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
      New_Line;
   end Print_Solution;

   -----------------
   -- Brute_Force --
   -----------------

   function Brute_Force (Costs : in Cost_Matrix) return Result_Matrix
   is
      Best_Cost   : Float := Float'Last;
      Best_Result : Result_Matrix (Costs'Range (1), Costs'Range (2));

      subtype Salesmen_Range is Integer range Costs'Range (1);
      subtype Cities is Integer range Costs'Range (2);
      subtype Unvisited_Cities is Cities range Cities'First + 1 .. Cities'Last;
      package Perm is new Permutations (Unvisited_Cities, Integer);
      --  Permutations goes from 2 .. Num_Cities, i.e. the unvisited cities.

      Num_Salesmen : constant Positive := Costs'Length (1);
      --  Num_Cities   : constant Positive := Costs'Length (2);

      --------------
      -- Try_Perm --
      --------------

      procedure Try_Perm (P : in Perm.Permutation) is
         Result : Result_Matrix (Best_Result'Range (1),
                                 Best_Result'Range (2)) :=
                                    (others => (others => 0));

         procedure Assign_Nth
           (P     : in Perm.Permutation;
            City  : in Natural; -- Index to P for next city to visit
            Pos   : in Pos_Matrix; -- Last city visited by some salesman
            Stage : in Stage_Matrix; -- Num of cities visited already by each salesman
            Res   : in Result_Matrix
           ) is
         begin
            if City > P'Last then
               --  Print_Solution (Costs, Res);
               --  We are at a leaf solution.
               if Best_Cost > Get_Total_Cost (Costs, Res) then
                  Best_Result := Res;
                  Best_Cost := Get_Total_Cost (Costs, Res);
               end if;
            else
               --  Try all assignations
               for S in Salesmen_Range loop
                  declare
                     Pos_Bis   : Pos_Matrix    := Pos;
                     Stage_Bis : Stage_Matrix  := Stage;
                     Res_Bis   : Result_Matrix := Res;
                  begin
                     Stage_Bis (S) := Stage (S) + 1; -- Salesman to next stage
                     Pos_Bis (S)   := P (City);      -- Salesman to new city
                     Res_Bis (S, Pos_Bis (S)) := Stage_Bis (S); -- Update result
                     Assign_Nth (P,
                                 City + 1,
                                 Pos_Bis,
                                 Stage_Bis,
                                 Res_Bis); -- Recursive call
                  end;
               end loop;
            end if;
         end Assign_Nth;

         use type Perm.Permutation;

      begin
         --  Prepare initial cities
         for I in Result'Range (1) loop
            Result (I, 1) := 1;
         end loop;

         Assign_Nth (P,
                     2,
                     (1 .. Num_Salesmen => 1),
                     (1 .. Num_Salesmen => 1),
                     Result);
         --  Start with no assignment.
      end Try_Perm;

      procedure Enumerate is new Perm.Enumeration (Try_Perm);

   begin
      pragma Assert (Costs'First (2) = Costs'First (3) and
                       Costs'Last (2) = Costs'Last (3));

      Enumerate;

      return Best_Result;
   end Brute_Force;

   --------------------
   -- Get_Total_Cost --
   --------------------

   function Get_Total_Cost (Costs : in Cost_Matrix;
                            Sol   : in Result_Matrix) return Float
   is
      Total_Cost : Float := 0.0;
   begin
      for S in Sol'Range (1) loop
         for I in Sol'Range (2) loop
            if Sol (S, I) > 1 then -- It's visited by this salesman
                                   --  locate previous location:
               for J in Sol'Range (2) loop
                  if Sol (S, J) = Sol (S, I) - 1 then
                     declare
                        Cost : constant Float := Costs (S, J, I);
                     begin
                        if Cost < Float'Last then
                           Total_Cost := Total_Cost + Cost;
                        else
                           return Float'Last;
                        end if;
                     end;
                  end if;
               end loop;
            end if;
         end loop;
      end loop;

      return Total_Cost;
   end Get_Total_Cost;

   ----------------------
   -- Get_Max_Min_Cost --
   ----------------------

   function Get_Max_Min_Cost (Costs : in Cost_Matrix;
                              Sol   : in Result_Matrix) return Float
   is
      Worst_Cost : Float := 0.0;
      Salesman_Cost : Float;
   begin
      for S in Sol'Range (1) loop
         Salesman_Cost := 0.0;
         for I in Sol'Range (2) loop
            if Sol (S, I) > 1 then -- It's visited by this salesman
                                   --  locate previous location:
               for J in Sol'Range (2) loop
                  if Sol (S, J) = Sol (S, I) - 1 then
                     declare
                        Cost : constant Float := Costs (S, J, I);
                     begin
                        if Cost < Float'Last then
                           Salesman_Cost := Salesman_Cost + Cost;
                        else
                           return Float'Last;
                        end if;
                     end;
                  end if;
               end loop;
            end if;
         end loop;
         if Salesman_Cost > Worst_Cost then
            Worst_Cost := Salesman_Cost;
         end if;
      end loop;

      return Worst_Cost;
   end Get_Max_Min_Cost;

end Agpl.Optimization.Mtsp;
