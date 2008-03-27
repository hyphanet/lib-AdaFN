with Agpl.Trace; use Agpl.Trace;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Unicode.Ccs.Iso_8859_1;

package body Agpl.Strings.Utf8 is

   -------------------
   -- Extract_Words --
   -------------------

   function Extract_Words (Str : String) return Containers.String_Vectors.Vector
   is
      Words   : Containers.String_Vectors.Vector;
      Str_Idx : Natural := Str'First;
      Word    : Ustring;
      use Asu;
   begin
      while Str_Idx <= Str'Last loop
         declare
            Char : Unicode.Unicode_Char;
         begin
            Unicode.Ces.Utf8.Read (Str, Str_Idx, Char);

            if Unicode.Is_Letter (Char) then
               declare
                  Code : String (1 .. 20);
                  Last : Natural := Code'First;
               begin
                  Unicode.Ces.Utf8.Encode (Char, Code, Last);
                  Append (Word, Code (Code'First + 1 .. Last));
               end;
            end if;

            if Str_Idx > Str'Last or else not Unicode.Is_Letter (Char) then
               if Word /= Null_Ustring then
                  Words.Append (+Word);
                  Word := Null_Ustring;
               end if;
            end if;
         end;
      end loop;

      return Words;
   exception
      when Constraint_Error =>
         Log ("Agpl.Str.Utf8 [C_E to Inv_Enc]", Warning);
         raise Unicode.Ces.Invalid_Encoding;
   end Extract_Words;

   ---------------------
   -- Is_Valid_Latin1 --
   ---------------------

   function Is_Valid_Latin1 (Str : Utf8_String) return Boolean is
      use Unicode;
   begin
      if Ces.Utf8.To_Unicode_Le
        (Str,
         Unicode.Ccs.Iso_8859_1.Iso_8859_1_Character_Set) /= ""
      then
         null;
      end if;
--      Log ("Utf8 is latin1: " & Str, Always);
      return True;
   exception
      when Unicode.Ccs.Invalid_Code =>
--         Log ("Utf8 is NOT latin1: " & Str, Always);
         return False;
   end Is_Valid_Latin1;

end Agpl.Strings.Utf8;
