 

with Agpl.Conversions;

package Agpl.Strings is

   pragma Preelaborate;

   ------------------------------------------------------------------------
   -- Contains                                                           --
   ------------------------------------------------------------------------
   --  Says if a string is substring of another:
   function Contains (Search_in, Search_for : in String) return Boolean;

   -----------
   -- Count --
   -----------
   function Count (S, Pattern : String) return Natural;
   --  Times that Pattern appears within S.

   -------------
   -- Replace --
   -------------
   function Replace (S, Pattern, New_Pattern : String) return String;
   --  Replace all occurrences

   ------------------------------------------------------------------------
   -- Pos                                                                --
   ------------------------------------------------------------------------
   --  Pos of Pattern in S, starting at S'First + Start - 1
   --  Returns 0 if not found
   function Pos (S : in String; Pattern : in String; Start : in Positive := 1)
      return Natural;

   ------------------------------------------------------------------------
   -- Lpad                                                               --
   ------------------------------------------------------------------------
   function Lpad (S    : in String;
                  Size : in Natural;
                  Fill : in Character := ' ') return String;
   pragma Inline (Lpad);

   ------------------------------------------------------------------------
   -- Rpad                                                               --
   ------------------------------------------------------------------------
   function Rpad (S    : in String;
                  Size : in Natural;
                  Fill : in Character := ' ') return String;
   pragma Inline (Rpad);

   ------------------------------------------------------------------------
   -- Starts                                                             --
   ------------------------------------------------------------------------
   --  Says if a string starts with some other:
   function Starts (Search_in, Prefix : in String) return Boolean;

   ------------------------------------------------------------------------
   -- To_lower                                                           --
   ------------------------------------------------------------------------
   function To_lower (This : in String) return String;
   function L        (This : in String) return String renames To_lower;
   pragma Inline (To_lower);

   ------------------------------------------------------------------------
   -- To_upper                                                           --
   ------------------------------------------------------------------------
   function To_upper (This : in String) return String;
   function U        (This : in String) return String renames To_upper;
   pragma Inline (To_upper);

   ----------------
   -- Capitalize --
   ----------------
   function Capitalize (This : in String) return String;
   --  Mixed case Xxxxxx_xxxx

   ------------------------------------------------------------------------
   -- To_string                                                          --
   ------------------------------------------------------------------------
   function To_String (N : Integer) return String
                       renames Conversions.To_String;
   function S (N : Integer) return String renames Conversions.To_String;
   function To_string (N        : Float;
                       Decimals : Natural := 2)
                       return     String renames Conversions.To_String;

   function To_string (N        : Long_Long_Float;
                       Decimals : Natural := 2)
                       return     String renames Conversions.To_String;

   ------------------------------------------------------------------------
   -- Trim                                                               --
   ------------------------------------------------------------------------
   function Trim (This : in String) return String
                  renames Conversions.Trim;

   ----------
   -- Left --
   ----------
   function Left (This : in String; Count : Natural) return String;

   function Crunch (This : String; Sep : Character := ' ') return String;
   --  Ensure that no two Sep chars appear by shrinking the excess ones
   --  e.g. "abc  de" --> "abc de"
   --  No outer trimming is performed

   function Untab (This : String) return String;
   --  Replace any tab characters with spaces (1 tab = 1 space)

end Agpl.Strings;
