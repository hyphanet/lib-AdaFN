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
-- Date:  26 August 1999                                           --
-- Edit:  13   July 2000 to use heap-allocated buffers rather than --
--                       stack arrays, and implement a changed     --
--                       interface to Write_IDAT_Chunk accordingly.--
--         1 November 2000 to add Ancillary parameter and code.    --
--         4     June 2002 to add ability to handle interlacing.   --
--         7     June 2002 to fix a bug in the handling of alpha   --
--                         values in sixteen bit images.           --
--         7     July 2004 to make changes consequent on use of    --
--                         Zlib Ada.                               --
--        25 November 2004 to use Sample'Pos consequent on change  --
--                         of the formal type to (<>).             --
--        13   August 2006 Added compression level parameter.      --
--        10 September 2006 to remove buffering of uncompressed    --
--                          data and perform compression and IDAT  --
--                          chunk output 'on-the-fly'.             --
---------------------------------------------------------------------
---------------------------------------------------------------------

separate(PNG_IO)
procedure Write_PNG_Type_6(Filename  : in String;
                           I         : in Image_Handle;
                           X, Y      : in Dimension;
                           Bit_Depth : in Depth_8_16 := Eight;
                           Interlace : in Boolean    := False;
                           Ancillary : in Chunk_List := Null_Chunk_List;
                           Level     : in Compression_Level := Default_Compression) is
  F : File_Type;

  Table : constant array(Depth_8_16) of Stream_Element_Offset := (4, 8);
  Bpp   : Stream_Element_Offset renames Table(Bit_Depth);
  function Filter is new Adaptive_Filter(Bpp);

  Compressor : Zlib.Filter_Type;

begin

  Start_File(Filename, X, Y, Six, Bit_Depth, Interlace, F, Compressor, Level);

  -- Write any ancillary chunks supplied that have to be positioned before the
  -- IDAT chunk(s).

  Write_Ancillary_Chunks(F, Ancillary, Before_PLTE);
  Write_Ancillary_Chunks(F, Ancillary, Before_IDAT);

  declare

    Header : Zlib_Header; -- Used to cache the header if necessary.
    Header_Status : Zlib_Header_Output_Status := Not_Yet_Seen;
  
    procedure Write_Compressed_Data is new
              Write_Compressed_IDAT_Data(F, Header, Header_Status);
  
    procedure Compress is new Zlib.Write(Write_Compressed_Data, Buffer_Size => IDAT_Size);

    -- The following procedure writes the image or a sub-image to the uncompressed
    -- data buffer. The Pass parameter default value is used when writing a whole
    -- (non-interlaced) image. In this case, it is not used in the procedure. The
    -- procedure cannot be called for a zero size sub-image, since its X and Y
    -- parameters are of type Dimension, for which the lower bound is 1.

    procedure Write_Image(X, Y : in Dimension; Pass : Pass_Number := 1) is
      Bps : constant Stream_Element_Count := Bytes_per_Scanline(Six, Bit_Depth, X);
      Scanline          : Stream_Element_Array(1 .. Bps);
      Previous_Scanline : Stream_Element_Array(1 .. Bps) := (others => 0);
    begin
      pragma Assert(Interlace or Pass = 1);
      for Row in 0 .. Coordinate(Y - 1) loop
        for Col in 0 .. Coordinate(X - 1) loop
          declare
            P : constant Stream_Element_Offset := 1 + Stream_Element_Offset(Col) * Bpp;
            R : Coordinate := Row;
            C : Coordinate := Col;
          begin
            if Interlace then
              R := Image_Row(Row, Pass); -- If the image is interlaced we must map from
              C := Image_Col(Col, Pass); -- sub-image to whole image coordinates.
            end if;
            case Bit_Depth is
              when Eight   =>
                Scanline(P .. P + 3) := Stream_Element(Sample'Pos(  Red_Sample(I, R, C)))
                                      & Stream_Element(Sample'Pos(Green_Sample(I, R, C)))
                                      & Stream_Element(Sample'Pos( Blue_Sample(I, R, C)))
                                      & Stream_Element(Sample'Pos(Alpha_Sample(I, R, C)));
              when Sixteen =>
                Scanline(P .. P + 7) := To_Buffer_2(Unsigned_16(Sample'Pos(  Red_Sample(I, R, C))))
                                      & To_Buffer_2(Unsigned_16(Sample'Pos(Green_Sample(I, R, C))))
                                      & To_Buffer_2(Unsigned_16(Sample'Pos( Blue_Sample(I, R, C))))
                                      & To_Buffer_2(Unsigned_16(Sample'Pos(Alpha_Sample(I, R, C))));
            end case;
          end;
        end loop;
        Compress(Compressor, Filter(Scanline, Previous_Scanline), Zlib.No_Flush);
        Previous_Scanline := Scanline;
      end loop;
    end Write_Image;

  begin

    if Interlace then
      for P in Pass_Number loop
        declare
          W : constant Natural := Sub_Image_Width (X, P);
          H : constant Natural := Sub_Image_Height(Y, P);
        begin
          if W > 0 and H > 0 then
            Write_Image(W, H, P);
          end if;
        end;
      end loop;
    else
      Write_Image(X, Y);
    end if;

    -- Flush all the IDAT data from the Zlib compressor to IDAT chunks in the file.

    while not Zlib.Stream_End(Compressor) loop
      Compress(Compressor, Null_Stream_Element_Array, Zlib.Finish); -- This may not be the best
	                                                                -- way to flush the data ..
    end loop;
    Zlib.Close(Compressor);

  end;

  -- Write any ancillary chunks supplied that may be positioned anywhere.
  
  Write_Ancillary_Chunks(F, Ancillary, Anywhere);

  Finish_File(F);
end Write_PNG_Type_6;
