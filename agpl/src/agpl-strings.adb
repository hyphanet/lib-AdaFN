 

with Ada.Characters.Handling;
with Ada.Strings;
with Ada.Strings.Fixed;

--  with Agpl.Text_Io; use Agpl.Text_Io;

package body Agpl.Strings is

   ------------------------------------------------------------------------
   -- Pos                                                                --
   ------------------------------------------------------------------------
   --  Pos of Pattern in S, starting at Start
   --  Returns 0 if not found
   function Pos (S : in String; Pattern : in String; Start : in Positive := 1)
      return Natural
   is
   begin
      return Ada.Strings.Fixed.Index (S (S'First + Start - 1 .. S'Last), Pattern);
   end Pos;

   -----------
   -- Count --
   -----------

   function Count (S, Pattern : String) return Natural is
      Aux   : constant String (1 .. S'Length) := S;
      Found : Natural := Pos (Aux, Pattern);
      Times : Natural := 0;
   begin
      while Found > 0 loop
         Times := Times + 1;
         Found := Pos (Aux, Pattern, Found + 1);
      end loop;
      return Times;
   end Count;

   ------------------------------------------------------------------------
   -- To_lower                                                           --
   ------------------------------------------------------------------------
   function To_lower (This : in String) return String is
   begin
      return Ada.Characters.Handling.To_lower (This);
   end To_lower;

   ------------------------------------------------------------------------
   -- To_upper                                                           --
   ------------------------------------------------------------------------
   function To_upper (This : in String) return String is
   begin
      return Ada.Characters.Handling.To_upper (This);
   end To_upper;

   ------------------------------------------------------------------------
   -- Lpad                                                               --
   ------------------------------------------------------------------------
   function Lpad (
      S    : in String;
      Size : in Natural;
      Fill : in Character := ' ') return String
   is
   begin
      return S & String'(1 .. Size - S'Length => Fill);
   end Lpad;

   ------------------------------------------------------------------------
   -- Rpad                                                               --
   ------------------------------------------------------------------------
   function Rpad (
      S    : in String;
      Size : in Natural;
      Fill : in Character := ' ') return String
   is
   begin
      return String'(1 .. Size - S'Length => Fill) & S;
   end Rpad;

   ------------------------------------------------------------------------
   -- Contains                                                           --
   ------------------------------------------------------------------------
   --  Says if a string is substring of another:
   function Contains (Search_in, Search_for : in String) return Boolean is
   begin
      return Ada.Strings.Fixed.Index (Search_in, Search_for) > 0;
   end Contains;

   ------------------------------------------------------------------------
   -- Starts                                                             --
   ------------------------------------------------------------------------
   --  Says if a string starts with some other:
   function Starts (Search_in, Prefix : in String) return Boolean is
   begin
      if Prefix'Length > Search_in'Length then
         return False;
      else
         return Search_in (Search_in'First .. Search_in'First + Prefix'Length - 1) = Prefix;
      end if;
   end Starts;

   -------------
   -- Replace --
   -------------

   function Replace (S, Pattern, New_Pattern : String) return String is
      Result : String (S'First .. S'First + S'Length * New_Pattern'Length);
      Pos    : Positive := Result'First;
      Wold   : constant Natural := Pattern'Length - 1;
      Wnew   : constant Natural := New_Pattern'Length - 1;
      I      : Positive := S'First;
   begin
      while I + Wold <= S'Last loop
         if S (I .. I + Wold) = Pattern then
            Result (Pos .. Pos + Wnew) := New_Pattern;
            I   := I   + Pattern'Length;
            Pos := Pos + New_Pattern'Length;
         else
            Result (Pos) := S (I);
            I   := I   + 1;
            Pos := Pos + 1;
         end if;
      end loop;

      Result (Pos .. Pos + S'Last - I) := S (I .. S'Last);
      Pos := Pos + S'Last - I + 1;

--        Put_Line ("Old:" & S);
--        Put_Line ("Pos:" & Pos'Img);
--        Put_Line ("New:" & Result);

      return Result (Result'First .. Pos - 1);

   end Replace;

   ----------
   -- Left --
   ----------

   function Left (This : in String; Count : Natural) return String is
   begin
      return This (This'First .. Natural'Min (This'Last, This'First + Count - 1));
   end Left;

   ----------------
   -- Capitalize --
   ----------------

   function Capitalize (This : in String) return String is
   begin
      return U (This (This'First .. This'First)) &
             L (This (This'First + 1 .. This'Last));
   end Capitalize;

   ------------
   -- Crunch --
   ------------

   function Crunch (This : String; Sep : Character := ' ') return String is
      R : String (This'Range);
      I : Positive := R'First;
      Skip : Boolean := False;
   begin
      for J in This'Range loop
         if This (J) = Sep then
            if not Skip then
               R (I) := This (J);
               I     := I + 1;
               Skip  := True;
            end if;
         else
            R (I) := This (J);
            I     := I + 1;
            Skip  := False;
         end if;
      end loop;
      return R (R'First .. I - 1);
   end Crunch;

   -----------
   -- Untab --
   -----------

   function Untab (This : String) return String is
   begin
      return Replace (This, "" & Character'Val (9), " ");
   end Untab;

end Agpl.Strings;
