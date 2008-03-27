 

with Ada.Numerics.Elementary_Functions;

package body Agpl.Cv is

   -------------------
   -- Inner_Product --
   -------------------

   function Inner_Product (L, R : Float_Array_3) return Float is
   begin
      return L (1) * R (1) + L (2) * R (2) + L (3) * R (3);
   end Inner_Product;

   -------------------
   -- Cross_Product --
   -------------------

   function Cross_Product (L, R : Float_Array_3) return Float_Array_3 is
   begin
      return
        (L (2) * R (3) - L (3) * R (2),
         L (3) * R (1) - L (1) * R (3),
         L (1) * R (2) - L (2) * R (1));
   end Cross_Product;

   --------------
   -- Distance --
   --------------

   function Distance (L, R : Point2D) return Float is
      use Ada.Numerics.Elementary_Functions;
   begin
      return Sqrt ((L (1) - R (1)) * (L (1) - R (1)) +
                   (L (2) - R (2)) * (L (2) - R (2)));
   end Distance;

   function Distance (Line : Line2D; Point : Point2D) return Float is
      use Ada.Numerics.Elementary_Functions;
      P : constant Point2D := Normalize (Point);
   begin
      return
        abs (Line (1) * P (1) + Line (2) * P (2) + Line (3)) /
        sqrt (Line (1) * Line (1) + Line (2) * Line (2));
   end Distance;

   ---------------------
   -- Signed_Distance --
   ---------------------

   function Signed_Distance (Line : Line2D; Point : Point2D) return Float is
      use Ada.Numerics.Elementary_Functions;
      P : constant Point2D := Normalize (Point);
   begin
      return
        (Line (1) * P (1) + Line (2) * P (2) + Line (3)) /
        sqrt (Line (1) * Line (1) + Line (2) * Line (2));
   end Signed_Distance;

   ---------------
   -- Normalize --
   ---------------

   function Normalize (Point : Point2D) return Point2D is
   begin
      if Point (3) = 0.0 or else Point (3) = 1.0 then
         return Point;
      else
         return (Point (1) / Point (3), Point (2) / Point (3), 1.0);
      end if;
   end Normalize;

end Agpl.Cv;
