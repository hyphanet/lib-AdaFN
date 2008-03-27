 

--  Drawing inside BMPs

with Agpl.Types.Constants;

package Agpl.Bmp.Draw is

   pragma Preelaborate;

   ------------------------------------------------------------------------
   -- Delete                                                             --
   ------------------------------------------------------------------------
   --  Fills the full BMP
   procedure Delete (
      This  : in out Object;
      Color : in     Types.RGB_Triplet := Types.Constants.Black);

   ------------------------------------------------------------------------
   -- Box                                                                --
   ------------------------------------------------------------------------
   --  Filled box
   procedure Box (
      This : in out Object;
      R1, C1,
      R2, C2 : in     Integer;
      Color  : in     Types.RGB_Triplet);

   ------------------------------------------------------------------------
   -- Circle                                                             --
   ------------------------------------------------------------------------
   procedure Circle (
      This  : in out Object;
      Row   : in     Integer;
      Col   : in     Integer;
      Rad   : in     Natural;
      Color : in     Types.RGB_triplet;
      Fill  : in     Types.RGB_triplet);

   ------------------------------------------------------------------------
   -- Circunference                                                      --
   ------------------------------------------------------------------------
   procedure Circunference (
      This  : in out Object;
      Row   : in     Integer;
      Col   : in     Integer;
      Rad   : in     Natural;
      Color : in     Types.RGB_triplet);

   ------------------------------------------------------------------------
   -- Line                                                               --
   ------------------------------------------------------------------------
   --  Bresenham algorithm for line drawing
   procedure Line (
      This   : in out Object;
      R1, C1,
      R2, C2 : in     Integer;
      Color  : in     Types.RGB_Triplet);

   ------------------------------------------------------------------------
   -- Plot                                                               --
   ------------------------------------------------------------------------
   --  Puts a single point
   procedure Plot (
      This  : in out Object;
      Row   : in     Integer;
      Col   : in     Integer;
      Color : in     Types.RGB_Triplet) renames Set_pixel;

private

   ------------------------------------------------------------------------
   -- Circ                                                               --
   ------------------------------------------------------------------------
   procedure Circ (
      This   : in out Object;
      Row    : in     Integer;
      Col    : in     Integer;
      Rad    : in     Natural;
      Color  : in     Types.RGB_triplet;
      Fill   : in     Boolean := False;
      FColor : in     Types.RGB_triplet := Types.Constants.Black);

end Agpl.Bmp.Draw;
