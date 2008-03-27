--  GENERIC PACKAGE FOR HANDLING PERMUTATIONS OF DISCRETE ITEMS
   -----------------------------------------------------------

--  Creation : 17-JAN-1989 by Mats Weber.


with Exchange;

package body Permutations is
-------------------------

   function Identity return Permutation is

      Result : Permutation;

   begin
      for A in Discrete loop
         Result(A) := A;
      end loop;
      return Result;
   end Identity;


   function To_Permutation (The_Cycle : Cycle) return Permutation is

      Result : Permutation := Identity;

   begin
      for I in The_Cycle.Contents'First..The_Cycle.Contents'Last - 1 loop
         Result(The_Cycle.Contents(I)) := The_Cycle.Contents(I + 1);
      end loop;
      Result(The_Cycle.Contents(The_Cycle.Contents'Last)) := The_Cycle.Contents(The_Cycle.Contents'First);
      return Result;
   end To_Permutation;


   function Equal (Left, Right : Cycle) return Boolean is
   begin
      return To_Permutation(Left) = To_Permutation(Right);
   end Equal;


   function "*" (Left : Permutation; Right : Permutation) return Permutation is

      Result : Permutation;

   begin
      for A in Discrete loop
         Result(A) := Left(Right(A));
      end loop;
      return Result;
   end "*";

   function "*" (Left : Permutation; Right : Cycle) return Permutation is
   begin
      return Left * To_Permutation(Right);
   end "*";

   function "*" (Left : Cycle; Right : Permutation) return Permutation is
   begin
      return To_Permutation(Left) * Right;
   end "*";

   function "*" (Left : Cycle; Right : Cycle) return Permutation is
   begin
      return To_Permutation(Left) * To_Permutation(Right);
   end "*";


   function Inverse (Of_Permutation : Permutation) return Permutation is

      Result : Permutation;

   begin
      for A in Discrete loop
         Result(Of_Permutation(A)) := A;
      end loop;
      return Result;
   end Inverse;

   function Inverse (Of_Cycle : Cycle) return Cycle is

      Result : Cycle(Of_Cycle.Length);

   begin
      for I in Of_Cycle.Contents'Range loop
         Result.Contents(Result.Contents'Last - I + Result.Contents'First) := Of_Cycle.Contents(I);
      end loop;
      return Result;
   end Inverse;


   function Decomposition (Of_Permutation : Permutation) return Cycle_List is

      Start_Of_Cycle   : Discrete := Discrete'First;
      A                : Discrete;
      Visited          : array (Discrete) of Boolean := (others => False);
      Cycle_Contents   : Discrete_List(1..Cycle_Length'Last);
      Length_Of_Cycle  : Count range 1..Cycle_Length'Last;
      Result           : Cycle_List(1..Permutation'Length / 2);
      Result_Length    : Count range 0..Result'Last := 0;

   begin
      Decomposition_Loop :
         loop
            A               := Start_Of_Cycle;
            Length_Of_Cycle := 1;
            loop
               Visited(A) := True;
               Cycle_Contents(Length_Of_Cycle) := A;
               A := Of_Permutation(A);
               exit when A = Start_Of_Cycle;
               Length_Of_Cycle := Length_Of_Cycle + 1;
            end loop;
            if Length_Of_Cycle > 1 then
               Result_Length := Result_Length + 1;
               Result(Result_Length) := (Length   => Length_Of_Cycle,
                                         Contents => Cycle_Contents(1..Length_Of_Cycle));
            end if;
            loop
               exit Decomposition_Loop when Start_Of_Cycle = Discrete'Last;
               Start_Of_Cycle := Discrete'Succ(Start_Of_Cycle);
               exit when not Visited(Start_Of_Cycle);
            end loop;
         end loop Decomposition_Loop;
      return Result(1..Result_Length);
   end Decomposition;

   function Decomposition (Of_Permutation : Permutation) return Transposition_List is

      Decomposition_In_Cycles : constant Cycle_List := Decomposition(Of_Permutation);

      subtype Index is Count range 0..Decomposition_In_Cycles'Last;

      function Concatenate (Last : Index) return Transposition_List is
      begin
         if Last = 0 then
            declare

               Null_List : Transposition_List(1..0);

            begin
               return Null_List;
            end;
         else
            return Concatenate(Last => Last - 1) & Decomposition(Of_Cycle => Decomposition_In_Cycles(Last));
         end if;
      end Concatenate;

   begin
      return Concatenate(Last => Decomposition_In_Cycles'Last);
   end Decomposition;

   function Decomposition (Of_Cycle : Cycle) return Transposition_List is

      Result : Transposition_List(1..Of_Cycle.Length - 1);

   begin
      for I in Result'Range loop
         Result(I).Contents(1) := Of_Cycle.Contents(1);
         Result(I).Contents(2) := Of_Cycle.Contents(I + 1);
      end loop;
      return Result;
   end Decomposition;


   function Signature (Of_Permutation : Permutation) return Parity is

      Decomposition_In_Cycles  : constant Cycle_List := Decomposition(Of_Permutation);
      Result                   : Parity := Even;

   begin
      for I in Decomposition_In_Cycles'Range loop
         Result := Result * Signature(Of_Cycle => Decomposition_In_Cycles(I));
      end loop;
      return Result;
   end Signature;

   function Signature (Of_Cycle : Cycle) return Parity is
   begin
      if Of_Cycle.Length mod 2 = 0 then
         return Odd;
      else
         return Even;
      end if;
   end Signature;


   function "*" (Left, Right : Parity) return Parity is
   begin
      if Left = Right then
         return Even;
      else
         return Odd;
      end if;
   end "*";


   procedure Enumeration is

      Current_Permutation : Permutation;
      Already_Taken       : array (Discrete) of Boolean := (others => False);

      procedure Set_Image_Of (A : in Discrete) is
      begin
         for B in Discrete loop
            if not Already_Taken(B) then
               Current_Permutation(A) := B;
               Already_Taken(B) := True;
               if A = Discrete'Last then
                  Action(Current_Permutation);
               else
                  Set_Image_Of(Discrete'Succ(A));
               end if;
               Already_Taken(B) := False;
            end if;
         end loop;
      end Set_Image_Of;

   begin
      Set_Image_Of(Discrete'First);
   end Enumeration;


   function Random_Permutation return Permutation is

      Result : Permutation := Identity;

      procedure Swap is new Exchange(Discrete);

   begin
      for J in reverse Discrete'Succ(Discrete'First)..Discrete'Last loop
         Swap(Result(J),
              Result(Discrete'Val(Uniform(Discrete'Pos(Discrete'First),
                                          Discrete'Pos(J)))));
      end loop;
      return Result;
   end Random_Permutation;

end Permutations;
