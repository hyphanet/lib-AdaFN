with Agpl.Cv; use Agpl.Cv;
with Agpl.Strings; use Agpl.Strings;

with Text_Io; use Text_Io;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Numerics; use Ada.Numerics;

procedure T002_Sigdist is
begin
   Put_Line ("Dist: " & To_String
               (Signed_Distance
                  ((0.0, 0.0, 1.0) ** (1.0, 0.0, 1.0),
                   (2.0, 0.0, 1.0))));
   Put_Line ("Dist: " & To_String
               (Signed_Distance
                  ((0.0, 0.0, 1.0) ** (1.0, 0.0, 1.0),
                   (2.0, 1.0, 1.0))));
   Put_Line ("Dist: " & To_String
               (Signed_Distance
                  ((0.0, 0.0, 1.0) ** (1.0, 0.0, 1.0),
                   (2.0, - 1.0, 1.0))));

   Put_Line ("Dist: " & To_String
               (Signed_Distance
                  ((0.0, 0.0, 1.0) ** (-1.0, -1.0, 1.0),
                   (0.0, -1.0, 1.0))));
exception
   when E : others =>
      Put_Line ("Exception: " & Exception_Information (E));
end T002_Sigdist;
