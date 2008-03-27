with Agpl.Conversions;
with Agpl.Ustrings; use Agpl.Ustrings;

with Ada.Characters.Handling;
with Ada.Strings.Fixed;

package body Agpl.URL is

   subtype Escape_Code is String (1 .. 2);

   Not_Escaped : constant Escape_Code := "  ";

   function Code (C : in Character) return Escape_Code;
   pragma Inline (Code);
   --  Returns hexadecimal code for character C

   subtype ASCII_7 is Character range Character'First .. Character'Val (127);
   type ASCII_7_Set is array (ASCII_7) of Escape_Code;

   function Build_Hex_Escape return ASCII_7_Set;
   --  Returns the table with pre-computed encoding for 7bits characters

   ----------------------
   -- Build_Hex_Escape --
   ----------------------

   function Build_Hex_Escape return ASCII_7_Set is
      Result : ASCII_7_Set;
   begin
      for C in Character'Val (0) .. Character'Val (127) loop
         if Strings.Maps.Is_In (C, Default_Encoding_Set) then
            Result (C) := Code (C);
         else
            Result (C) := Not_Escaped;
         end if;
      end loop;
      return Result;
   end Build_Hex_Escape;

   ----------
   -- Code --
   ----------

   function Code (C : in Character) return Escape_Code is
   begin
      return Conversions.To_Hex (Character'Pos (C));
   end Code;

   Hex_Escape : constant ASCII_7_Set :=  Build_Hex_Escape;
   --  Limit Hex_Escape to 7bits ASCII characters only. Other ISO-8859-1 are
   --  handled separately in Encode function. Space character is not processed
   --  specifically, contrary to what is done in AWS.URL.

   ------------
   -- Decode --
   ------------

   function Decode (Str : in String) return String is
      Res : String (1 .. Str'Length);
      K   : Natural := 0;
      I   : Positive := Str'First;
   begin
      if Str = "" then
         return "";
      end if;

      loop
         K := K + 1;

         if Str (I) = '%'
           and then I + 2 <= Str'Last
           and then Characters.Handling.Is_Hexadecimal_Digit (Str (I + 1))
           and then Characters.Handling.Is_Hexadecimal_Digit (Str (I + 2))
         then
            Res (K) := Conversions.From_Hex (Str (I + 1 .. I + 2));
            I := I + 2;

         elsif Str (I) = '+' then
            Res (K) := ' ';
         else
            Res (K) := Str (I);
         end if;

         I := I + 1;
         exit when I > Str'Last;
      end loop;

      return Res (1 .. K);
   end Decode;

   ------------
   -- Encode --
   ------------

   function Encode
     (Str          : in String;
      Encoding_Set : in Strings.Maps.Character_Set := Default_Encoding_Set)
      return String
   is
      C_128 : constant Character := Character'Val (128);
      Res   : String (1 .. Str'Length * 3);
      K     : Natural := 0;
   begin
      for I in Str'Range loop
         if not
           (Str (I) = '%'
            and then I + 2 <= Str'Last
            and then Characters.Handling.Is_Hexadecimal_Digit (Str (I + 1))
            and then Characters.Handling.Is_Hexadecimal_Digit (Str (I + 2)))
         and then Strings.Maps.Is_In (Str (I), Encoding_Set) then
            --  This character must be encoded

            K := K + 1;
            Res (K) := '%';
            K := K + 1;

            if Str (I) < C_128 then
               --  We keep a table for characters lower than 128 for efficiency
               Res (K .. K + 1) := Hex_Escape (Str (I));
            else
               Res (K .. K + 1) := Code (Str (I));
            end if;

            K := K + 1;

         else
            K := K + 1;
            Res (K) := Str (I);
         end if;
      end loop;

      return Res (1 .. K);
   end Encode;

   -----------------------
   -- Requires_Encoding --
   -----------------------

   function Requires_Encoding
     (Str : String;
      Set : Strings.Maps.Character_Set := Requiring_Encoding_Set)
      return Boolean
   is
   begin
      for I in Str'Range loop
         if Strings.Maps.Is_In (Str (I), Set) then
            return True;
         end if;
         if Str (I) = '%'
           and then not
             (I + 2 <= Str'Last
              and then Characters.Handling.Is_Hexadecimal_Digit (Str (I + 1))
              and then Characters.Handling.Is_Hexadecimal_Digit (Str (I + 2)))
         then
            return True;
         end if;
      end loop;
      return False;
   end Requires_Encoding;

   ---------------
   -- Normalize --
   ---------------

   function Normalize (Url : in String) return String is
      Url_Path : Ustring := +Url;

      K        : Natural;
      P        : Natural;

      use Asu;
   begin
      --  Checks for current directory and removes all occurences

      --  Look for starting ./

      if Length (URL_Path) >= 2 and then Slice (URL_Path, 1, 2) = "./" then
         Delete (URL_Path, 1, 1);
      end if;

      --  Look for all /./ references

      loop
         K := Index (URL_Path, "/./");

         exit when K = 0;

         Delete (URL_Path, K, K + 1);
      end loop;

      --  Checks for parent directory

      loop
         K := Index (URL_Path, "/../");

         exit when K = 0;

         --  Look for previous directory, which should be removed

         P := Strings.Fixed.Index
           (Slice (URL_Path, 1, K - 1), "/", Strings.Backward);

         exit when P = 0;

         Delete (URL_Path, P, K + 2);
      end loop;

      return +URL_Path;
   end Normalize;

end Agpl.URL;
