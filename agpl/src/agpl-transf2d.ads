 

--  Transformations for translations/rotations in a 2D environment.
--  Doing Matrix * Vector' cause the described effects.
--  Doing Vector * Matrix  cause ???

generic
   type Real is digits <>;
package Agpl.Transf2D is

   pragma Pure;

   Matrix_Singular : exception;

   type Real_Array  is array (Integer'Base range <>) of Real;
   type Real_Matrix is array (Integer'Base range <>,
                              Integer'Base range <>) of Real;

   type Pose is new Real_Array (1 .. 4);
   --  X, Y, Angle, Homogeneous component.

   X : constant := Pose'First;
   Y : constant := Pose'First + 1;
   A : constant := Pose'First + 2;
   H : constant := Pose'Last;

   Null_Pose : constant Pose := (X => 0.0, Y => 0.0, A => 0.0, H => 1.0);
   Origin    :          Pose renames Null_Pose;

   type Transformation is new Real_Matrix (1 .. 4, 1 .. 4);

   Identity : constant Transformation;

   function Decompose (AB, AC : in Pose) return Pose; -- BC
   pragma Inline (Decompose);
   function Invert_Compose (AB, AC : in Pose) return Pose renames Decompose;
   function Relative (AB, AC : in Pose) return Pose renames Decompose; -- BC
   function Transf_AB_AC_BC (AB, AC : in Pose) return Pose renames Decompose;
   --  Given two poses set in the same reference frame, get the second
   --  as seen from the first pose.
   --  NOTE: this is equivalent to Compose (Invert (AB), AC))

   function Get_Decomposition (P1 : in Pose) return Transformation;
   pragma Inline (Get_Decomposition);
   --  Get the transformation used by Decompose
   --  I.e. Get_Decomposition (AB) * AC = Decompose (AB, AC)
   --  I.e. Get_Inverse (Get_Composition (AB)) * AC = Decompose (AB, AC)

   function Compose (AB, BC : in Pose) return Pose; -- AC
   pragma Inline (Compose);
   function Absolute (AB, BC : in Pose) return Pose renames Compose; -- AC
   function Transf_AB_BC_AC (AB, BC : in Pose) return Pose renames Compose;
   --  Given a pose in some ref and a second pose in P1 ref, obtain
   --  P2 in the global ref of P1.

   function Get_Composition (P1 : in Pose) return Transformation;
   pragma Inline (Get_Composition);
   function Get_Transf_AB (AB : in Pose) return Transformation
                           renames Get_Composition;
   --  Get the transformation used by Compose
   --  I.e. Get_Composition (AB) * BC = Compose (AB, BC)

   function Invert_AB_BA (AB : in Pose) return Pose; -- BA
   pragma Inline (Invert_AB_BA);
   function Inverse (AB : in Pose) return Pose renames Invert_AB_BA; -- BA
   function Invert  (AB : in Pose) return Pose renames Invert_AB_BA; -- BA
   --  Given a pose B in ref A, return A as seen from B

   function Get_Transf_BA (AB : in Pose) return Transformation;
   pragma Inline (Get_Transf_BA);
   --  Get the transformation used by Invert
   --  I.e Get_Transf_BA (AB) * AB = Invert_AB_BA (AB)
   --  This is a particular case of Decompose where AC = Origin.

   function Get_Translation (TX, TY : in Real) return Transformation;
   pragma Inline (Get_Translation);
   --  Matrix who provides given translation.
   --  This moves the reference frame, not the pose.

   function Get_Rotation (A : in Real) return Transformation;
   pragma Inline (Get_Rotation);
   --  Matrix who provides given rotation.
   --  This moves the reference frame, not the pose.

   function Get_Rot_Trans (A, Tx, Ty : in Real) return Transformation;
   pragma Inline (Get_Rot_Trans);
   --  Optimization of Get_Rotation (A) * Get_Translation (Tx, Ty)
   --  Don't forget that this means that the translation will be applied first
   --  to the right vector, and then the translation

   function Get_Trans_Rot (Tx, Ty, A : in Real) return Transformation;
   pragma Inline (Get_Trans_Rot);
   --  Optimization of Get_Translation (Tx, Ty) * Get_Rotation (A)

   function Homogeneize (V : in Pose) return Pose;
   pragma Inline (Homogeneize);
   --  Apply the homogeneization if V (V'Last) /= 1.0

   function Inverse (X : in Transformation) return Transformation;
   --  Functionally, gives the inverse transformation of a given one.

   --  Inverse a matrix. May raise Matrix_Singular but shouldn't happen for
   --  proper transformation matrices.
   --  This routine was adapted from code taken from the Public Ada Library
   --  (PAL), at the following location:
   --
   --  http://wuarchive.wustl.edu/languages/ada/userdocs/html/cardcat/matrix.htm

   function "*" (M : in Transformation; V : in Pose) return Pose;
   --  Matrix * Vector.

   function "*" (V : in Pose; M : in Transformation) return Pose;
   --  Vector * Matrix.

   function "*" (M1, M2 : in Transformation) return Transformation;
   --  Matrix * Matrix.

   function To_String (V : in Pose) return String;

private

   Identity : constant Transformation :=
                ((1.0, 0.0, 0.0, 0.0),
                 (0.0, 1.0, 0.0, 0.0),
                 (0.0, 0.0, 1.0, 0.0),
                 (0.0, 0.0, 0.0, 1.0));

end Agpl.Transf2D;
