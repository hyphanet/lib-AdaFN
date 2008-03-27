with Agpl.Bag;
with Text_Io; use Text_Io;
with Ada.Exceptions; use Ada.Exceptions;

procedure T020_Bag is
   package Char_Bag is new Agpl.Bag (Character, Integer, Initial_Size => 2);
   use Char_Bag;

   B : Object (First => 1);

   procedure Print (This : in Object) is
   begin
      for I in This.First .. This.Last loop
         Put (I'Img & ":" & This.Vector (I));
      end loop;
      New_Line;
   end Print;

   Flag : Boolean := True;

   procedure Moving (C        : in out Character;
                     Context  : in out Integer;
                     From, To : in Integer)
   is
      pragma Unreferenced (Context);
   begin
      Put_Line (C & ":" & From'Img & " ->" & To'Img);
      if Flag then
         B.Append ('f');
         B.Append ('g');
         B.Append ('h');
         Flag := False;
      end if;
   end Moving;

begin
   B.Append ('a'); B.Append ('b'); B.Append ('c'); B.Append ('d'); B.Append ('e');
   Print (B);
   B.Delete (2, Moving'Access);
   Print (B);
   B.Insert ('i', 2, Moving'Access);
   Print (B);
exception
   when E : others =>
      Put_Line ("Exception: " & Exception_Information (E));
end T020_Bag;
