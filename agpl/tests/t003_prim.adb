with Agpl.Boost.Graphs;
with Agpl.Interfaces.C.Types;

with Ada.Text_Io; use Ada.Text_Io;

procedure T003_Prim is
   procedure Test_C;
   pragma Import (C, Test_C, "agpl__boost__test_prim");
begin
   Test_C;

   New_Line;

   declare
      use Agpl.Interfaces.C.Types;
      W : constant Double_Arrays.C_Matrix :=
            (1 => (0.0, 5.0, 1.0),
             2 => (5.0, 0.0, 2.0),
             3 => (1.0, 2.0, 0.0));
      T : constant Double_Arrays.C_Matrix := Agpl.Boost.Graphs.Prim (W);
   begin
      for I in T'Range (1) loop
         for J in T'Range (2) loop
            if I < J and then T (I, J) < Agpl.Boost.Graphs.Inf_Weight then
               Put_Line ("TREE:" & Integer'Image (I) & " --" &
                         Integer'Image (J) & " [W:" &
                         Integer'Image (Integer (T (I, J))) & "]");
            end if;
         end loop;
      end loop;
   end;
end T003_Prim;
