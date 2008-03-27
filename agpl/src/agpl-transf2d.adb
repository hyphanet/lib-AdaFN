 

with Ada.Numerics.Generic_Elementary_Functions;

package body Agpl.Transf2D is

   Optimized : constant Boolean := True;

   package Math is new Ada.Numerics.Generic_Elementary_Functions (Real);
   use Math;

   -------------
   -- Compose --
   -------------

   function Compose (AB, BC : in Pose) return Pose is
   begin
      return Get_Composition (AB) * BC;
   end Compose;

   ---------------------
   -- Get_Composition --
   ---------------------

   function Get_Composition (P1 : in Pose) return Transformation is
   begin
      return Get_Trans_Rot (-P1 (X), -P1 (Y), -P1 (A));
   end Get_Composition;

   ---------------
   -- Decompose --
   ---------------

   function Decompose (AB, AC : in Pose) return Pose is
   begin
      return Get_Decomposition (AB) * AC;
   end Decompose;

   -----------------------
   -- Get_Decomposition --
   -----------------------

   function Get_Decomposition (P1 : in Pose) return Transformation is
   begin
      return Get_Rot_Trans (P1 (A), P1 (X), P1 (Y));
   end Get_Decomposition;

   ---------------------
   -- Get_Translation --
   ---------------------

   function Get_Translation (TX, TY : in Real) return Transformation is
   begin
      return
        ((1.0, 0.0, 0.0, -TX),
         (0.0, 1.0, 0.0, -TY),
         (0.0, 0.0, 1.0, 0.0),
         (0.0, 0.0, 0.0, 1.0));
   end Get_Translation;

   ------------------
   -- Get_Rotation --
   ------------------

   function Get_Rotation (A : in Real) return Transformation is
      CS : constant Real := Cos (A);
      SN : constant Real := Sin (A);
   begin
      return
        (( CS,  SN, 0.0, 0.0),
         (-SN,  CS, 0.0, 0.0),
         (0.0, 0.0, 1.0,  -A),
         (0.0, 0.0, 0.0, 1.0));
   end Get_Rotation;

   -------------------
   -- Get_Rot_Trans --
   -------------------

   function Get_Rot_Trans (A, Tx, Ty : in Real) return Transformation is
      CS : constant Real := Cos (A);
      SN : constant Real := Sin (A);
   begin
      return
        (( CS,  SN, 0.0, -CS * Tx - SN * Ty),
         (-SN,  CS, 0.0,  SN * Tx - CS * Ty),
         (0.0, 0.0, 1.0,                 -A),
         (0.0, 0.0, 0.0,                1.0));
   end Get_Rot_Trans;

   -------------------
   -- Get_Trans_Rot --
   -------------------

   function Get_Trans_Rot (Tx, Ty, A : in Real) return Transformation is
      CS : constant Real := Cos (A);
      SN : constant Real := Sin (A);
   begin
      return
        (( CS,  SN, 0.0, -Tx),
         (-SN,  CS, 0.0, -Ty),
         (0.0, 0.0, 1.0, - A),
         (0.0, 0.0, 0.0, 1.0));
   end Get_Trans_Rot;

   -----------------
   -- Homogeneize --
   -----------------

   function Homogeneize (V : in Pose) return Pose is
      H : Real renames V (Transf2d.H);
   begin
      if H /= 0.0 and then H /= 1.0 then
         return (V (X) / H,
                 V (Y) / H,
                 V (A), -- Angle, not to normalize.
                 1.0);
      else
         return V;
      end if;
   end Homogeneize;

   ------------------
   -- Invert_AB_BA --
   ------------------

   function Invert_AB_BA (AB : in Pose) return Pose is
   begin
      if not Optimized then
         return Decompose (AB, Origin);
      else
         declare
            CS : constant Real := Cos (AB (A));
            SN : constant Real := Sin (AB (A));
         begin
            return
              (-CS * AB (X) - SN * AB (Y),
                SN * AB (X) - CS * AB (Y),
                                  -AB (A),
                                      1.0);
         end;
      end if;
   end Invert_AB_BA;

   -------------------
   -- Get_Transf_BA --
   -------------------

   function Get_Transf_BA (AB : in Pose) return Transformation is
      CS : constant Real := Cos (AB (A));
      SN : constant Real := Sin (AB (A));
   begin
      return ((-CS, -SN, 0.0, 0.0),
              ( SN, -CS, 0.0, 0.0),
              (0.0, 0.0, -1.0, 0.0),
              (0.0, 0.0, 0.0, 1.0));
   end Get_Transf_BA;

   -------------
   -- Inverse --
   -------------

   function Inverse (X : in Transformation) return Transformation is
      --  This routine was adapted from code taken from the Public Ada Library
      --  (PAL), at the following location:
      --
      --  http://wuarchive.wustl.edu/languages/ada/userdocs/html/cardcat/matrix.htm
      B                        : Transformation;
      Result                   : Transformation;
      I_PIVOT, J_PIVOT         : Integer range 1 .. X'Length (1);
      BIG_ENTRY, TEMP, EPSILON : Real := 0.0;
      L, M                     : array (1 .. X'Length (1)) of Integer;

   begin
      B := X;

      --  initiate the Row and Column interchange information

      for K in  B'Range (1) loop
         L (K) := K; -- Row interchage information
         M (K) := K; -- Column interchange information
      end loop;

      --  major loop for inverse

      for K in  B'Range (1) loop

         --  & search for Row and Column Integer I_PIVOT, J_PIVOT
         --  & both in (K .. B'LAST(1) ) for maximum B(I,J)
         --  & in absolute value :BIG_ENTRY

         BIG_ENTRY := 0.0;
         --
         --  check Real_Matrix singularity
         --
         for I in  K .. B'Last (1) loop
            for J in  K .. B'Last (1) loop
               if abs (B (I, J)) > abs (BIG_ENTRY) then
                  BIG_ENTRY := B (I, J);
                  I_PIVOT   := I;
                  J_PIVOT   := J;
               end if;
            end loop;
         end loop;
         if K = B'First (1) then
            if BIG_ENTRY = 0.0 then
               raise Matrix_Singular;
            else
               EPSILON := REAL (B'Length (1)) *
                          abs (BIG_ENTRY) *
                          0.000001;
            end if;
         else
            if abs (BIG_ENTRY) < EPSILON then
               raise Matrix_Singular;
            end if;
         end if;

         --  interchange Row and Column

         --  & interchange K-th and I_PIVOT-th Rows
         if I_PIVOT /= K then
            for J in  B'Range (1) loop
               TEMP           := B (I_PIVOT, J);
               B (I_PIVOT, J) := B (K, J);
               B (K, J)       := TEMP;
            end loop;
            L (K) := I_PIVOT;
         end if;
         --& interchange K-th and J_PIVOT-th Columns
         if J_PIVOT /= K then
            for I in  B'Range (1) loop
               TEMP           := B (I, J_PIVOT);
               B (I, J_PIVOT) := B (I, K);
               B (I, K)       := TEMP;
            end loop;
            M (K) := J_PIVOT;
         end if;

         --  & divide K-th Column by minus pivot (-BIG_ENTRY)

         for I in  B'Range (1) loop
            if I /= K then
               B (I, K) := B (I, K) / (-BIG_ENTRY);
            end if;
         end loop;

         --  reduce Real_Matrix Row by Row

         for I in  B'Range (1) loop
            if I /= K then
               for J in  B'Range (1) loop
                  if J /= K then
                     B (I, J) := B (I, J) + B (I, K) * B (K, J);
                  end if;
               end loop;
            end if;
         end loop;

         --  & divide K-th Row by pivot

         for J in  B'Range (1) loop
            if J /= K then
               B (K, J) := B (K, J) / BIG_ENTRY;
            end if;
         end loop;
         B (K, K) := 1.0 / BIG_ENTRY;

      end loop; -- end of major inverse loop

      --  final Column and Row interchange to obtain
      --  inverse of X, i.e. X**(-1)

      for K in reverse  B'Range (1) loop
         --  Column interchage
         J_PIVOT := L (K);
         if J_PIVOT /= K then
            --  & intechange B(I,J_PIVOT) and B(I,K) for each Row I
            for I in  B'Range (1) loop
               TEMP           := B (I, J_PIVOT);
               B (I, J_PIVOT) := B (I, K);
               B (I, K)       := TEMP;
            end loop;
         end if;
         --  Row interchage
         I_PIVOT := M (K);
         if I_PIVOT /= K then
            --  & INTECHANGE B(I_PIVOT,J) and B(K,J) for each Column J
            for J in  B'Range (1) loop
               TEMP           := B (I_PIVOT, J);
               B (I_PIVOT, J) := B (K, J);
               B (K, J)       := TEMP;
            end loop;
         end if;
      end loop;

      --  inverse of X is obtained and stored in B

      Result := B;

      return Result;
   end Inverse;

   ---------
   -- "*" --
   ---------

   function "*" (M : in Transformation; V : in Pose) return Pose is
      R : Pose := (0.0, 0.0, 0.0, 0.0);
   begin
      for I in V'Range loop
         for J in M'Range (2) loop
            R (I) := R (I) + M (I, J) * V (J);
         end loop;
      end loop;

      return R;
   end "*";

   function "*" (V : in Pose; M : in Transformation) return Pose is
      R : Pose := (0.0, 0.0, 0.0, 0.0);
   begin
      for I in V'Range loop
         for J in M'Range (1) loop
            R (I) := R (I) + M (J, I) * V (J);
         end loop;
      end loop;

      return R;
   end "*";

   function "*" (M1, M2 : in Transformation) return Transformation is
      Rs : Transformation := (others => (others => 0.0));
   begin
      for R in Rs'Range (1) loop
         for C in Rs'Range (2) loop
            for I in Rs'Range (1) loop
               Rs (R, C) := Rs (R, C) + M1 (R, I) * M2 (I, C);
            end loop;
         end loop;
      end loop;

      return Rs;
   end "*";

   ---------------
   -- To_String --
   ---------------

   function To_String (V : in Pose) return String is
      type P is delta 0.01 digits 18;
   begin
      return
      "(" & P'Image (P (V (1))) &
      "," & P'Image (P (V (2))) &
      "," & P'Image (P (V (3))) &
      "," & P'Image (P (V (4))) & ")";
   end To_String;

end Agpl.Transf2D;
