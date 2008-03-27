with Text_Io; use Text_Io;
with Ada.Exceptions; use Ada.Exceptions;

with Permutations;

procedure T005_Permutations is
   type Int is new Integer range 1 .. 4;

   package Perm is new Permutations (Int, Integer);

   procedure Print (P : in Perm.Permutation) is
   begin
      for I in P'Range loop
         Put (P (I)'Img);
      end loop;
      New_Line;
   end Print;

   procedure Enumerate is new Perm.Enumeration (Print);

begin
   Enumerate;
exception
   when E : others =>
      Put_Line ("Exception: " & Exception_Information (E));
end T005_Permutations;
