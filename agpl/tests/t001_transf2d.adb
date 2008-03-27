with Test_Pkg;

with Text_Io; use Text_Io;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Numerics; use Ada.Numerics;

procedure T001_Transf2d is

   use Test_Pkg.Transf2DFloat;

begin
   declare
      Initial  : constant Pose := (1.0, 0.0, 0.0, 1.0);
   begin
      Put_Line ("45 rot: " & To_String (Get_Rotation (Pi / 4.0) * Initial));
      Put_Line ("90 rot: " & To_String (Get_Rotation (Pi / 2.0) * Initial));
      Put_Line ("135 rot: " & To_String (Get_Rotation (3.0 * Pi / 4.0) * Initial));
      Put_Line ("180 rot: " & To_String (Get_Rotation (Pi) * Initial));
      Put_Line ("270 rot: " & To_String (Get_Rotation (3.0 * Pi / 2.0) * Initial));
   end;

   Put_Line ("Decomposeosition: " &
             To_String
               (Compose ((4.0, 4.0, 0.0, 1.0),
                           (1.0, 1.0, 0.0, 1.0))));
   Put_Line ("Decomposeosition: " &
             To_String
               (Compose ((4.0, 4.0, 0.0, 1.0),
                           (-4.0, 0.0, 0.0, 1.0))));
   Put_Line ("Decomposeosition: " &
             To_String
               (Compose ((4.0, 4.0, Pi / 4.0, 1.0),
                           (1.0, 1.0, 0.0, 1.0))));
   Put_Line ("Decomposeosition: " &
             To_String
               (Compose ((4.0, 4.0, Pi / 4.0, 1.0),
                           ( - 4.0, 0.0, 0.0, 1.0))));
   Put_Line ("Decomposeosition: " &
             To_String
               (Compose ((-4.0, 3.0, Pi / 4.0, 1.0),
                           ( - 2.0, - 2.0, Pi / 2.0, 1.0))));

   New_Line;

   Put_Line ("Composeosition: " &
             To_String
               (Decompose ((1.0, 1.0, 0.0, 1.0),
                         (2.0, 1.0, 0.0, 1.0))));
   Put_Line ("Composeosition: " &
             To_String
               (Decompose ((1.0, 1.0, 0.0, 1.0),
                         (2.0, 2.0, 0.0, 1.0))));
   Put_Line ("Composeosition: " &
             To_String
               (Decompose ((1.0, 1.0, Pi / 4.0, 1.0),
                         (-1.0, 1.0, 0.0, 1.0))));

   New_Line;

   Put_Line ("Invert: " &
             To_String (Invert_AB_BA ((1.0, 1.0, 0.0, 1.0))));
   Put_Line ("Invert: " &
             To_String (Invert_AB_BA ((1.0, 1.0, Pi / 4.0, 1.0))));
   Put_Line ("Invert: " &
             To_String (Invert_AB_BA ((1.0, 1.0, Pi / 2.0, 1.0))));
   Put_Line ("Invert: " &
             To_String (Invert_AB_BA ((1.0, 1.0, Pi, 1.0))));

   New_Line;

   Put_Line ("Invert: " &
             To_String (Get_Transf_BA ((1.0, 1.0, Pi / 4.0, 1.0)) *
                          (1.0, 1.0, Pi / 4.0, 1.0)));
   Put_Line ("Decomp: " &
             To_String (Get_Decomposition ((1.0, 1.0, Pi / 4.0, 1.0)) *
                          (0.0, 0.0, 0.0, 1.0)));

   New_Line;

   Put_Line (To_String (Get_Composition ((2.0, 2.0, 0.0, 1.0)) *
                        Get_Decomposition ((1.0, 1.0, 0.5, 1.0)) *
                          (1.0, 1.0, 0.5, 1.0)));
exception
   when E : others =>
      Put_Line ("Exception: " & Exception_Information (E));
end T001_Transf2d;
