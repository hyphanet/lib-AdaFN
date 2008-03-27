 

with Agpl.Containers.String_Vectors;

with Unicode.Ces.Utf8;

package Agpl.Strings.Utf8 is

   subtype Utf8_String is Unicode.Ces.Utf8.Utf8_String;

   function Extract_Words (Str : String) return Containers.String_Vectors.Vector;
   --  Will split the string using non-letters and non-numbers as delimiters

   function Is_Valid_Latin1 (Str : Utf8_String) return Boolean;
   --  Says if this utf8 string encodes a valid latin1 string

end Agpl.Strings.Utf8;
