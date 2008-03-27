 

--  Root package for Computer Vision

package Agpl.Cv is

   pragma Pure;

   type Float_Array is array (Positive range <>) of Float;

   type Float_Array_3 is new Float_Array (1 .. 3);

   function Inner_Product (L, R : Float_Array_3) return Float;

   function "*"           (L, R : Float_Array_3) return Float renames Inner_Product;

   function Cross_Product (L, R : Float_Array_3) return Float_Array_3;
   --  The types aren't that important because of the duality principle.
   --  This is arguable but for convenience will have it like this.

   function "**"          (L, R : Float_Array_3) return Float_Array_3
                           renames Cross_Product;

   type Point2D is new Float_Array_3;
   type Line2D  is new Float_Array_3;

   function Distance      (L, R : Point2D) return Float;
   --  Euclidean distance between two points.

   function Distance      (Line : Line2D; Point : Point2D) return Float;
   --  Shortest distance from a point to a line.

   function Signed_Distance (Line : Line2D; Point : Point2D) return Float;
   --  Distance is signed to mean at what side the point is of the line.
   --  If line is A x B, being above AB is positive, below is negative.

   function Normalize     (Point : Point2D) return Point2D;
   --  Ensure that P (3) = 1.0, unless it is 0.0

private

   pragma Inline (Inner_Product, Cross_Product, Distance, Normalize);

end Agpl.Cv;
