with Text_Io; use Text_Io;
with Ada.Exceptions; use Ada.Exceptions;

procedure T019_Passing is

   type Junk is array (1 .. 1000) of Integer;

   type Thing is tagged record
      X    : Integer := 15;
      Fill : Junk := (others => 16#Dead#);
   end record;

   function Pass_Thru (This : Thing) return Thing is
   begin
      return This;
   end Pass_Thru;

   procedure Update (This : in out Thing) is
   begin
      This.X := 16#fff#;
   end Update;

begin
   declare
      A : Thing;
      B : Thing := A;
   begin
      Put_Line
        (Boolean'Image
           (Pass_Thru
              (Pass_Thru
                 (Pass_Thru
                    (A))).X
            = 15));

      Update (A);
   end;
exception
   when E : others =>
      Put_Line ("Exception: " & Exception_Information (E));
end T019_Passing;
