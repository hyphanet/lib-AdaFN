with Text_Io; use Text_Io;
with Ada.Exceptions; use Ada.Exceptions;

with Agpl.G2.Packet;
use  Agpl.G2;
with Agpl.Streams;
use  Agpl.Streams;

procedure T007_g2p is
   P : Packet.Object := Packet.Create ("ADDTASK");
   C : Packet.Object := Packet.Create ("DESCR", "asldkfjkldrn");
begin
   P.Add_Child (C);
   declare
      Data : constant Stream_Element_Array := P.To_Stream_Element_Array;
   begin
      Put_Line ("Data length is" & Data'Length'Img);
   end;
exception
   when E : others =>
      Put_Line ("Exception: " & Exception_Information (E));
end T007_g2p;
