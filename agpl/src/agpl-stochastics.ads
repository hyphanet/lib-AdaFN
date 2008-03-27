 

--  Root package for stochastics packages.

package Agpl.Stochastics is

   pragma Pure;

   --  The following types could be made generic if necessary at this level?

   Delta_Error : constant := 0.000001;
   --  Distance to 1.0 in total probabilities greater than this would be
   --  considered an error:

   type Probabilities is new Float range 0.0 .. 1.0 + Delta_Error;

end Agpl.Stochastics;
