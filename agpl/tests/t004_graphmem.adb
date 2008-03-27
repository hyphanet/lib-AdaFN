with Text_Io; use Text_Io;
with Ada.Exceptions; use Ada.Exceptions;

procedure T004_GraphMem is
begin
   null;
exception
   when E : others =>
      Put_Line ("Exception: " & Exception_Information (E));
end T004_GraphMem;
