---------------------------------------------------------------------
---------------------------------------------------------------------
-- PNG_IO -- Ada95 Portable Network Graphics Input/Output Package  --
--                                                                 --
-- Copyright (©) 2001 Dr Stephen J. Sangwine (S.Sangwine@IEEE.org) --
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
-- Date:     12    March 2001                                      --
-- Modified: 18 December 2002 to correct spelling error in comment --
--           13   August 2006 to permit the sample types to be     --
--                            discrete rather than limiting them   --
--                            to modular.                          --
---------------------------------------------------------------------
---------------------------------------------------------------------
-- This package implements the non-linear encoding required in     --
-- sRGB PNG images. It provides transformations between linear RGB --
-- sample values and the non-linear 8-bit RGB values which are     --
-- stored in sRGB PNGs.                                            --
---------------------------------------------------------------------
---------------------------------------------------------------------

generic

  -- The type provided for the linear samples should have more than
  -- 8 bits (that is it should be at least mod 2**9). The type for
  -- the sRGB samples must have at least 8 bits, but the maximum
  -- encoded value will be 255.

  type Linear_Sample is (<>);
  type   sRGB_Sample is (<>);

package PNG_IO.Standard_RGB_Encodings is

  subtype Encoded_Sample is sRGB_Sample range sRGB_Sample'Val(0) ..
                                              sRGB_Sample'Val(255);

  function sRGB_Encode(X :  Linear_Sample) return Encoded_Sample;
  function sRGB_Decode(X : Encoded_Sample) return  Linear_Sample;

end PNG_IO.Standard_RGB_Encodings;
