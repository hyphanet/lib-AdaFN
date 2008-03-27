with Agpl.Containers.Unbounded_Trees;
with Agpl.Text_Io; use Agpl.Text_Io;
with Agpl.Trace; use Agpl.Trace;

with Ada.Finalization; use Ada.Finalization;

procedure T024_Utrees is
   type Sides is (L, C, R);

   type Int is new Controlled with record
      I : Integer;
   end record;

   function "+" (I : Integer) return Int is
   begin
      return (Controlled with I => I);
   end "+";

   function "+" (X, Y : Int) return Int is
   begin
      return (Controlled with I => X.I + Y.I);
   end "+";

   package Side_Trees is
     new Agpl.Containers.Unbounded_Trees (Int, Sides, "<");
   use Side_Trees;

   procedure Merge is new Side_Trees.Merge ("+");

begin
   Put_Line ("Starting...");
   declare
      T, U, V, W : Side_Trees.Tree;

      function Image (I : Int) return String is
      begin
         return I.I'Img;
      end Image;

      procedure Print is new Side_Trees.Print (Image, Sides'Image);
   begin
      T.Root.Insert (+1);
      T.Root.Child (L).Insert (+2);
      T.Root.Child (R).Insert (+5);
      T.Root.Child (L).Child (C).Insert (+3);

      Put_Line ("Dump follows:");
      Print (T);
      Put_Line ("Dump end.");

      U.Root.Insert (+2);
      U.Root.Child (L).Insert (+0);
      U.Root.Child (L).Child (L).Insert (+1);
      U.Root.Child (C).Insert (+2);
      U.Root.Child (R).Insert (+1);
      U.Root.Child (R).Child (C).Insert (+1);

      New_Line; Put_Line ("U dump follows:");
      Print (U);

      New_Line; Put_Line ("W (empty) follows:");
      Print (W);

      T.Root.Copy (W);
      New_Line; Put_Line ("W (copied) follows:");
      Print (W);
      Merge (W, U);

      New_Line;
      Put_Line ("Merge follows:");
      Print (W);

      New_Line;
      Put_Line ("T app U follows:");

      T.Root.Copy (V);
      V.Root.Child (L).Child (C).Child (R).Insert (U);
      Print (V);
   end;
   Put_Line ("Done.");
exception
   when E : others =>
      Put_Line (Report (E));
      Log ("Main: " & Report (E), Error);
end T024_Utrees;
