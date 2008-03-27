with Ada.Streams;           use Ada.Streams;
with Ada.Streams.Stream_Io; use Ada.Streams.Stream_Io;
with Ada.Text_Io;           use Ada.Text_Io;

procedure B001_Pack is

   type Bool is (F, T);
   type Bit_Array is array (Positive range <>, Positive range <>) of Bool;
   pragma Pack (Bit_Array);

   X : constant Bit_Array (1 .. 3, 1 .. 3) := ((F, T, T), (F, F, T), (F, T, F));

   procedure Print (X : Bit_Array) is
   begin
      for I in X'Range loop
         for J in X'Range (2) loop
            Put_Line (I'Img & J'Img & " " & X (I, J)'Img);
         end loop;
      end loop;
   end Print;

   Fi : Ada.Streams.Stream_Io.File_Type;
begin
   Print (X);

   Create (Fi, Out_File, "b001_pack");
   Bit_Array'Output (Stream (Fi), X);
   Close (Fi);

   Open (Fi, In_File, "b001_pack");
   declare
      Y : constant Bit_Array := Bit_Array'Input (Stream (Fi));
   begin
      Close (Fi);
      Print (Y);
      Put_Line (Boolean'Image (X = Y));
   end;
end B001_Pack;
