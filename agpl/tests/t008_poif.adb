with Text_Io; use Text_Io;
with Ada.Exceptions; use Ada.Exceptions;

with Agpl.Smart_Access;

procedure T008_poif is
   type Ia is access all Integer;
   package Blah is new Agpl.Smart_Access (Integer, Ia);
begin
   null;
exception
   when E : others =>
      Put_Line ("Exception: " & Exception_Information (E));
end T008_poif;
