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
-- Date:  1 November 2000                                          --
-- Edit:  9  January 2004 to comment out the unused body of the    --
--                        function After_PLTE.                     --
--       13   August 2006 to refer to the ISO standard and not the --
--                        draft.                                   --
---------------------------------------------------------------------
---------------------------------------------------------------------
-- This package body contains the rules for placement of ancillary --
-- chunks relative to PLTE and IDAT chunks. This is needed when    --
-- writing these chunks to PNG files. A concise description of the --
-- rules is given in the ISO standard Section 5.6, Table 5.3.      --
---------------------------------------------------------------------
---------------------------------------------------------------------

separate(PNG_IO)
package body Chunk_Ordering is

  type Chunk_Position is (Before, After, Anywhere);

  type Chunk_Info is
    record
      Chunk_Name : Unsigned_32;
      PLTE, IDAT : Chunk_Position;
    end record;

  Table : constant array(Positive range <>) of Chunk_Info :=
      -----------------------------
      --  Name   PLTE      IDAT  --
      -----------------------------
        ((cHRM, Before,   Before  ),
         (gAMA, Before,   Before  ),
         (iCCP, Before,   Before  ),
         (sBIT, Before,   Before  ),
         (sRGB, Before,   Before  ),
         (bKGD, After,    Before  ), 
         (hIST, After,    Before  ), 
         (tRNS, After,    Before  ), 
         (pHYS,	Anywhere, Before  ),
         (sPLT,	Anywhere, Before  ), 
         (tIME, Anywhere, Anywhere), 
         (iTXt, Anywhere, Anywhere), 
         (tEXt, Anywhere, Anywhere), 
         (zTXt, Anywhere, Anywhere));

  function Known_Chunk(C : Unsigned_32) return Boolean is
    Result : Boolean := False;
  begin
    for I in Table'Range loop
      Result := Result or (Table(I).Chunk_Name = C);
    end loop;
    return Result;
  end Known_Chunk;

  function Before_PLTE(C : Unsigned_32) return Boolean is
  begin
    for I in Table'Range loop
      if Table(I).Chunk_Name = C then
        return Table(I).PLTE = Before;
      end if;
    end loop;
    return False; -- If not found we assume the ordering doesn't matter.
  end Before_PLTE;

  -- The next function is not currently used, and is commented out to
  -- prevent compiler warnings about unused code.

  --function After_PLTE(C : Unsigned_32) return Boolean is
  --begin
  --  for I in Table'Range loop
  --    if Table(I).Chunk_Name = C then
  --      return Table(I).PLTE = After;
  --    end if;
  --  end loop;
  --  return False; -- If not found we assume the ordering doesn't matter.
  --end After_PLTE;

  function Before_IDAT(C : Unsigned_32) return Boolean is
  begin
    for I in Table'Range loop
      if Table(I).Chunk_Name = C then
        return Table(I).IDAT = Before;
      end if;
    end loop;
    return False; -- If not found we assume the ordering doesn't matter.
  end Before_IDAT;

end Chunk_Ordering;

