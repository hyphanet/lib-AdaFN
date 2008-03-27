---------------------------------------------------------------------
---------------------------------------------------------------------
-- PNG_IO -- Ada95 Portable Network Graphics Input/Output Package  --
--                                                                 --
-- Copyright (©) 1999 Dr Stephen J. Sangwine (S.Sangwine@IEEE.org) --
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
-- Created:   23 March    1999                                     --
-- Modified:  29 February 2000 to delete redundant declarations.   --
--            20 November 2000 to add handling of ancillary chunks.--
--            30      May 2002 to add handling of interlacing.     --
--             2     June 2003 to change Chunk at line 178 to a    --
--                             constant (which it was, in fact).   --
--            10  January 2004 to report the Zlib version in use.  --
--            19     July 2004 to remove the exception handler and --
--                             allow the default action to occur.  --
---------------------------------------------------------------------
---------------------------------------------------------------------
-- Test code for the PNG_IO package. Reads a PNG image and writes  --
-- a PNG file of the same type with the same pixel data (but       --
-- without any auxiliary chunks found in the input file).          --
---------------------------------------------------------------------
---------------------------------------------------------------------

with Ada.Command_Line, Ada.Text_IO, Ada.Integer_Text_IO;
use  Ada.Command_Line, Ada.Text_IO, Ada.Integer_Text_IO;

with PNG_IO;
use  PNG_IO;

procedure PNG_Test is

  Filename1 : constant String := Argument(1); -- A PNG image.
  Filename2 : constant String := Argument(2); -- A PNG of the same image.

  F : PNG_File;

  L : Chunk_List := Null_Chunk_List;

begin

  Put_Line("PNG_IO version " & Version);
  Put_Line("Zlib   version " & Zlib_Version);

  Put_Line("Opening file: " & Filename1);
  Open(F, Filename1); -- Open the PNG input file.

  declare
    W : constant Dimension        := Width(F);
    H : constant Dimension        := Height(F);
    D : constant Depth            := Bit_Depth(F);
    T : constant Colour_Type_Code := Colour_Type(F);
    I : constant Boolean          := Interlaced(F);
  begin
    Put_Line("Image information from file: " & Filename1);
    Put("Width                  "); Put(W); New_Line;
    Put("Height                 "); Put(H); New_Line;
    declare
      package Depth_IO is new Enumeration_IO(Depth);
      use     Depth_IO;
      package Colour_Type_IO is new Enumeration_IO(Colour_Type_Code);
      use     Colour_Type_IO;
      package Boolean_IO is new Enumeration_IO(Boolean);
      use     Boolean_IO;
    begin
      Put("Bit Depth              "); Put(D); New_Line;
      Put("Colour type:           "); Put(T); New_Line;
      Put("Interlaced:            "); Put(I); New_Line;
      Put("Palette:               ");
        if Palette(F) then
          Put(Palette_Size(F)); Put(" entries.");
        else
          Put("NONE");
        end if;
        New_Line;
      Put("Gamma:                 "); if Gamma(F) then
                                        Put(Gamma_Value(F));
                                        L := L & Gamma_Chunk(Gamma_Value(F));
                                      else
                                        Put("NONE");
                                      end if; New_Line;
      Put("Standard RGB:          "); if Standard_RGB(F) then
                                        Put("Yes: ");
                                        declare
                                          R : constant Rendering_Intent := SRGB_Rendering(F);
                                        begin
                                          L := L & Standard_RGB_Chunk(R);
                                          Put(Rendering_Intent'Image(R));
                                        end;
                                      else
                                        Put("No.");
                                      end if; New_Line;
      Put("Chroma:                "); 
        if Chromaticity(F) then
          declare
            W : constant Pair :=   White_Point(F);
            R : constant Pair :=   Red_Primary(F);
            G : constant Pair := Green_Primary(F);
            B : constant Pair :=  Blue_Primary(F);
          begin
            L := L & Chromaticity_Chunk(W, R, G, B);
            declare
              C : constant Positive_Count := Col(Standard_Output);
            begin
                          Put(W.X); Put(W.Y);
              Set_Col(C); Put(R.X); Put(R.Y);
              Set_Col(C); Put(G.X); Put(G.Y);
              Set_Col(C); Put(B.X); Put(B.Y);
            end;
          end;
        else
          Put("NONE");
        end if; New_Line;
      Put("Physical:              "); if Physical(F) then
                                        declare
                                          V : constant Pair := Physical_Value(F);
                                        begin
                                          L := L & Physical_Chunk(V, Unit_Meter(F));
                                          declare
                                            C : constant Positive_Count :=
                                                  Col(Standard_Output);
                                          begin
                                            Put("Unit unknown: "); Put(Unit_Unknown(F));
                                            Set_Col(C);
                                            Put("Unit meter  : "); Put(Unit_Meter(F));
                                            Set_Col(C);
                                            Put(V.X); Put(V.Y);
                                          end;
                                        end;
                                      else
                                        Put("NONE");
                                      end if; New_Line;
      Put("Number of text chunks: "); Put(NText(F));        New_Line;
      for I in 1 .. NText(F) loop
        declare
          Keyword : constant String := Text_Keyword(F, I);
          Text    : constant String := Text_String (F, I);
        begin
          if Keyword /= "Software" then
            -- Copy the chunk to the output file only if it is not "Software".
            L := L & Text_Chunk(Keyword, Text);
          end if;
          Put("Keyword:" & Keyword); New_Line;
          Put("Text   :" & Text   ); New_Line;
        end;
      end loop;
      declare
        A : constant Natural := Ancillary_Chunk_Count(F);
      begin
        Put("Number of unrecognised ancillary chunks: "); Put(A); New_Line;
        for I in 1 .. A loop
          if A = 1 then Put_Line("Name | Chunk length"); end if;
          declare
            C : constant Chunk := Ancillary_Chunk(F, I);
          begin
            L := L & C; -- Append the chunk to the list for output.
            Put(Name(C) & " | "); Put(Data(C)'Length); New_Line;
          end;
        end loop; New_Line;
      end;
    end;

    -- Output a PNG file so that the image data can be verified (visually)
    -- and also test the Write procedure for this image type. We do not
    -- need to read the image into memory, as we can fetch pixels from the
    -- input image and immediately write them out again. We use the types
    -- in PNG_IO for the generic parameters of the write procedures to
    -- simplify the code.

    declare
      procedure Write_0 is new Write_PNG_Type_0(PNG_File, Natural, Pixel_Value);
      procedure Write_2 is new Write_PNG_Type_2(PNG_File, Natural, 
                                                Red_Value, Green_Value, Blue_Value);
      procedure Write_3 is new Write_PNG_Type_3(PNG_File, Palette_Size, Natural, Natural,
                                                Palette_R_Value, Palette_G_Value,
                                                Palette_B_Value, PNG_File, Natural, Pixel_Value);
      procedure Write_4 is new Write_PNG_Type_4(PNG_File, Natural, 
                                                Pixel_Value, Alpha_Value);
      procedure Write_6 is new Write_PNG_Type_6(PNG_File, Natural, 
                                                Red_Value, Green_Value, Blue_Value,
                                                Alpha_Value);
    begin
      Put_Line("Reading pixel values and writing to file: " & Filename2); New_Line;
      case T is
        when Zero  => Write_0(Filename2, F, W, H, D, I, L);
        when Two   => Write_2(Filename2, F, W, H, D, I, L);
        when Three => Write_3(Filename2, F, F, W, H, I, L);
        when Four  => Write_4(Filename2, F, W, H, D, I, L);
        when Six   => Write_6(Filename2, F, W, H, D, I, L);
      end case;
    end;
  end;

  Destroy(L);
  Close(F);

end PNG_Test;
