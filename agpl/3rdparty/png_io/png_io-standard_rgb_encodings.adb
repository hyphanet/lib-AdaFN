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
-- Date:  12 March 2001                                            --
---------------------------------------------------------------------
---------------------------------------------------------------------

with Ada.Numerics.Elementary_Functions;

package body PNG_IO.Standard_RGB_Encodings is

  Encoding_LUT : array( Linear_Sample) of Encoded_Sample;
  Decoding_LUT : array(Encoded_Sample) of  Linear_Sample;

  function sRGB_Encode(X : Linear_Sample) return Encoded_Sample is
  begin
    return Encoding_LUT(X);
  end sRGB_Encode;

  function sRGB_Decode(X : Encoded_Sample) return Linear_Sample is
  begin
    return Decoding_LUT(X);
  end sRGB_Decode;

begin

  declare

    subtype Normalized_Float is Float range 0.0 .. 1.0;

    function Non_Linear(X : Normalized_Float) return Normalized_Float is
      use Ada.Numerics.Elementary_Functions;
    begin
      if X <= 0.003_04 then
        return 12.92 * X;
      else
        return 1.055 * (X ** (1.0/2.4)) - 0.055;
      end if;
    end Non_Linear;

    function Linear(X : Normalized_Float) return Normalized_Float is
      use Ada.Numerics.Elementary_Functions;
    begin
      if X <= 0.039_28 then
        return X/12.92;
      else
        return ((X + 0.055)/1.055) ** 2.4;
      end if;
    end Linear;

    ESL : constant Float := Float(Encoded_Sample'Pos(Encoded_Sample'Last));
    SL  : constant Float := Float( Linear_Sample'Pos( Linear_Sample'Last));

  begin

    for I in Encoding_LUT'range loop
      Encoding_LUT(I) := Encoded_Sample'Val(Integer(ESL * Non_Linear(Float(Linear_Sample'Pos(I))/SL)));
    end loop;

    for I in Decoding_LUT'range loop
      Decoding_LUT(I) := Linear_Sample'Val(Integer(SL * Linear(Float(Encoded_Sample'Pos(I))/ESL)));
    end loop;

  end;

end PNG_IO.Standard_RGB_Encodings;
