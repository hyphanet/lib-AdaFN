 

with Ada.Numerics;

--  Values of general use across Agpl

package Agpl.Types.Constants is

   pragma Pure;

   ----------
   -- Math --
   ----------
   Pi         : constant := Ada.Numerics.Pi;
   Pi_2       : constant := Pi / 2.0;
   Pi_4       : constant := Pi / 4.0;
   Two_Pi     : constant := 2.0 * Pi;
   Three_Pi_4 : constant := Pi * 3.0 / 4.0;
   Three_Pi_2 : constant := Pi * 3.0 / 2.0;

   --------------
   -- Sections --
   --------------
   --  For debugging
   CR  : constant String := "Cooperative robotics";
   CV  : constant String := "Computer vision";
   HTN : constant String := "Hierarchical task networks";

   ------------------------------------------------------------------------
   -- Colours                                                            --
   ------------------------------------------------------------------------
   --  Primary
   White  : constant RGB_Triplet := (255, 255, 255);
   Black  : constant RGB_Triplet := (  0,   0,   0);
   Red    : constant RGB_Triplet := (255,   0,   0);
   Green  : constant RGB_Triplet := (  0, 255,   0);
   Blue   : constant RGB_Triplet := (  0,   0, 255);

   --  Secondary
   Yellow : constant RGB_Triplet := (255, 255,   0);

   --  Web
   Navy      : constant RGB_Triplet := (  0,   0, 128);
   Gray      : constant RGB_Triplet := (128, 128, 128);
   Silver    : constant RGB_Triplet := (192, 192, 192);
   Gainsboro : constant RGB_Triplet := (220, 220, 220);

   Soft_Green     : constant RGB_Triplet := (  0, 192,   0);
   Middle_Green   : constant RGB_Triplet := (  0, 128,   0);
   Dark_Green     : constant RGB_Triplet := (  0, 096,   0);

   Soft_Red       : constant RGB_Triplet := (192,   0,   0);
   Middle_Red     : constant RGB_Triplet := (128,   0,   0);
   Dark_Red       : constant RGB_Triplet := (096,   0,   0);
   Crimson        : constant RGB_Triplet := (128,   0,   0);

end Agpl.Types.Constants;
