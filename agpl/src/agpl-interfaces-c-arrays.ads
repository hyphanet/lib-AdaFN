generic
   type Element is private;
package Agpl.Interfaces.C.Arrays is

   pragma Pure;

   type C_Array is array (Positive range <>) of aliased Element;
   pragma Convention (C, C_Array);

   type C_Matrix is array (Positive range <>,
                           Positive range <>) of aliased Element;
   pragma Convention (C, C_Matrix);

   function To_Array (This : C_Matrix) return C_Array;
   --  in row first order

   function To_Matrix (This : C_Array) return C_Matrix;
   --  must have quadratic size

end Agpl.Interfaces.C.Arrays;
