---------------------------------------------------------------------
---------------------------------------------------------------------
-- PNG_IO -- Ada95 Portable Network Graphics Input/Output Package  --
--                                                                 --
-- Copyright (©) 2004 Dr Stephen J. Sangwine (S.Sangwine@IEEE.org) --
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
-- Released:  1   July 2004                                        --
-- Edited:   13 August 2006 to permit the input and output sample  --
--                          parameters to be discrete rather than  --
--                          modular.                               --
---------------------------------------------------------------------
---------------------------------------------------------------------
-- This package implements gamma encoding between arbitrary gamma  --
-- values. The gamma values are encoded in the same way as in PNG  --
-- files so that values read from a file may be used directly to   --
-- instantiate this package. The package uses lookup tables which  --
-- are initialised at elaboration time.                            --
--                                                                 --
-- The input and output samples are represented by discrete types: --
-- the package uses the full range of the actual subtypes to       --
-- construct its lookup tables. The first value of the actual      --
-- subtype must be zero. This is checked by an assertion.          --
---------------------------------------------------------------------
---------------------------------------------------------------------

generic

  type  Input_Sample is (<>); -- These two types may be the same,
  type Output_Sample is (<>); -- but they are not required to be.

   Input_Gamma : in Natural;    -- These values represent the gamma
  Output_Gamma : in Natural;    -- multiplied by 100_000.

package PNG_IO.Gamma_Encoding is

  function To_Output_Gamma(I : Input_Sample) return Output_Sample;
  
end PNG_IO.Gamma_Encoding;

