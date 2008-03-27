---------------------------------------------------------------------
---------------------------------------------------------------------
-- PNG_IO -- Ada95 Portable Network Graphics Input/Output Package  --
--                                                                 --
-- Copyright (©) 2000 Dr Stephen J. Sangwine (S.Sangwine@IEEE.org) --
--                                                                 --
-- This software was created by Stephen J. Sangwine. He hereby     --
-- asserts his Moral Right to be identified as author of this      --
-- software.                                                       --
---------------------------------------------------------------------
---------------------------------------------------------------------
-- PNG_IO is free software; you can redistribute it and/or modify  --
-- it under the terms of the GNU General Public License as         --
-- published by the Free Software Foundation; either version 2 of  --
-- the License, or (at your option) any later version.             --
--                                                                 --
-- PNG_IO is distributed in the hope that it will be useful, but   --
-- WITHOUT ANY WARRANTY; without even the implied warranty of      --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the    --
-- GNU General Public License for more details.                    --
--                                                                 --
-- You should have received a copy of the GNU General Public       --
-- License along with this software (in the file gpl.txt); if not, --
-- contact the Free Software Foundation, or access www.fsf.org.    --
---------------------------------------------------------------------
---------------------------------------------------------------------
-- Created:   18     July 2000                                     --
-- Modified:   6 February 2001 to fix stupid error! The first file --
--                             was read twice and the second file  --
--                             was ignored. How did I miss that?   --
--             7     June 2002 to output the actual pixel values   --
--                             which do not match.                 --
--             3  January 2002 to output the largest difference in --
--                             sample values if non-zero.          --
--            22    March 2004 to output the Zlib version as well  --
--                             as the PNG_IO version.              --
--            20     July 2004 to remove the exception handler.    --
--            10 September 2006 to set exit status.                --
---------------------------------------------------------------------
---------------------------------------------------------------------
-- PNG image comparison program. Reads two PNG image files and     --
-- verifies that they contain the same pixel data. If the two PNGs --
-- were written by different coders, the comparison validates one  --
-- coder against the other for correctness of the pixel data. This --
-- can be used to check PNG_IO against other PNG coders. It checks --
-- colour palettes for Type 3 PNGs only indirectly, since it gets  --
-- the pixel values after looking up the palette. If any pixels    --
-- differ, the values are output.                                  --
---------------------------------------------------------------------
---------------------------------------------------------------------

with Ada.Command_Line, Ada.Text_IO;
use  Ada.Command_Line, Ada.Text_IO;

with PNG_IO;
use  PNG_IO;

procedure PNG_Compare is

  Filename1 : constant String := Argument(1); -- Two PNGs of the
  Filename2 : constant String := Argument(2); -- same image.

  F1, F2 : PNG_File;

begin

  Put_Line("Opening file: " & Filename1);
  Open(F1, Filename1);

  Put_Line("Opening file: " & Filename2);
  Open(F2, Filename2);

  declare
    W1 : constant Dimension        := Width(F1);
    W2 : constant Dimension        := Width(F2);
    H1 : constant Dimension        := Height(F1);
    H2 : constant Dimension        := Height(F2);
    D1 : constant Depth            := Bit_Depth(F1);
    D2 : constant Depth            := Bit_Depth(F2);
    T1 : constant Colour_Type_Code := Colour_Type(F1);
    T2 : constant Colour_Type_Code := Colour_Type(F2);
  begin
    Put_Line("PNG_IO version " & Version);
    Put_Line("Zlib   version " & Zlib_Version);
    if W1 /= W2 or H1 /= H2 or D1 /= D2 or T1 /= T2 then
      Put_Line("The two files are not of the same size, bit depth or PNG type.");
      Set_Exit_Status(Failure);
      return;
    end if;

    -- The two files seem to be compatible. Read all the pixel information and
    -- compare, and output a message if any pixel or alpha values do not match.

    declare
      subtype Row_Coordinate is Coordinate range 0 .. H1 - 1;
      subtype Col_Coordinate is Coordinate range 0 .. W1 - 1;

      function Coordinate_Image(R : Row_Coordinate;
                                C : Col_Coordinate) return String is
      begin
        return '(' & Row_Coordinate'Image(R) & ','
                   & Col_Coordinate'Image(C) & ')';
      end Coordinate_Image;

      All_Pixels_Match : Boolean := True;

      subtype String_5 is String(1 .. 5);

      Maximum_Difference : Natural := 0;

      generic
        Label : in String_5;
        with function Value(F : PNG_File; R, C : Coordinate) return Natural;
      procedure Check(R, C : Coordinate);

      procedure Check(R, C : Coordinate) is
        V1 : constant Natural := Value(F1, R, C);
        V2 : constant Natural := Value(F2, R, C);
      begin
        if V1 /= V2 then
          Put_Line(Label & " values do not match at: " & Coordinate_Image(R, C) &
                   ": " & Natural'Image(V1) & ' ' & Natural'Image(V2));
          All_Pixels_Match := False;
          declare
            Difference : constant Integer := Integer(V1) - Integer(V2);
          begin
            Maximum_Difference := Natural'Max(Maximum_Difference, Natural(abs Difference));
          end;
        end if;
      end Check;

      procedure Check_Red   is new Check("Red  ",   Red_Value);
      procedure Check_Green is new Check("Green", Green_Value);
      procedure Check_Blue  is new Check("Blue ",  Blue_Value);
      procedure Check_Alpha is new Check("Alpha", Alpha_Value);
      procedure Check_Grey  is new Check("Grey ", Pixel_Value);

    begin
      for R in Row_Coordinate loop
        for C in Col_Coordinate loop
          if Colour(T1) then
            Check_Red  (R, C);
            Check_Green(R, C);
            Check_Blue (R, C);
          else
            Check_Grey (R, C);
          end if;
          if Alpha(T1) then
            Check_Alpha(R, C);
          end if;
        end loop;
      end loop;
      if All_Pixels_Match then
        Put_Line("No pixel value differences encountered.");
        Set_Exit_Status(Success);
      else
        Put_Line("Maximum difference in samples was: " & Natural'Image(Maximum_Difference));
        Set_Exit_Status(Failure);
      end if;
      New_Line;
    end;

    Close(F1);
    Close(F2);

  end;

end PNG_Compare;
