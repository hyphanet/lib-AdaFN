with Ada.Numerics.Elementary_Functions;

package body Agpl.Interfaces.C.Arrays is

   --------------
   -- To_Array --
   --------------

   function To_Array (This : C_Matrix) return C_Array is
      R : C_Array (1 .. This'Length (1) * This'Length (2));
      I : Positive := R'First;
   begin
      for Row in This'Range (1) loop
         for Col in This'Range (2) loop
            R (I) := This (Row, Col);
            I := + 1;
         end loop;
      end loop;

      return R;
   end To_Array;

   ---------------
   -- To_Matrix --
   ---------------

   function To_Matrix (This : C_Array) return C_Matrix is
      use Ada.Numerics.Elementary_Functions;

      R : C_Matrix (1 .. Natural (Sqrt (Float (This'Length))),
                    1 .. Natural (Sqrt (Float (This'Length))));
      I : Positive := This'First;
   begin
      for Row in R'Range (1) loop
         for Col in R'Range (2) loop
            R (Row, Col) := This (I);
            I := I + 1;
         end loop;
      end loop;

      return R;
   end To_Matrix;

end Agpl.Interfaces.C.Arrays;
