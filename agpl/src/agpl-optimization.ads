 

package Agpl.Optimization is

   pragma Preelaborate;

   --  type Cost is delta 0.00001 digits 18;
   --  type Cost is new Long_Float;
   type Cost is new Float;
   --  We are *minimizing* the solution cost.
   --  If you want ot maximize an utility function, simply negate your function.

   function Image (C : in Cost; Decimals : in Natural := 2) return String;

   Infinite : constant Cost := Cost'Last;

end Agpl.Optimization;
