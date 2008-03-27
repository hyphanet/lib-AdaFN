with Agpl.Strings; use Agpl.Strings;
with Agpl.Strings.Utf8;
with Agpl.Strings.Fields; use Agpl.Strings.Fields;
with Agpl.Trace; use Agpl.Trace;
with Agpl.Url;
use Agpl;

--  with Ada.Characters.Handling;

package body Adafn.Key is

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Object) return Boolean is
   begin
      return Image (L) < Image (R);
   end "<";

   ----------
   -- Clip --
   ----------

   function Clip (S : String) return String is
      begin
         if S (S'Last) = '/' then
            return S (S'First .. S'Last - 1);
         else
            return S;
         end if;
   end Clip;

   --------------
   -- Assemble --
   --------------
   --  Any key assembled here is valid (has proper form).
   --  It could still have bad crypto
   function Assemble (Kind     : Kinds;
                      Crypto   : Crypto_String;
                      Path     : String;
                      Revision : Integer := 0) return Object
   is
      --  use Ada.Characters.Handling;
      New_Path : Ustring := +Path;
      pragma Unreferenced (Path);
   begin
      --  Univocally have only one form of path representation
      if Url.Requires_Encoding (+New_Path) then
         New_Path := +Url.Encode (+New_Path);
      end if;

      --  Correct bad crypto
      if Contains (Crypto, "%7E") then
         return Assemble (Kind,
                          Replace (Crypto, "%7E", "~"),
                          +New_Path, Revision);
      elsif Contains (Crypto, "%7e") then
         return Assemble (Kind,
                          Replace (Crypto, "%7e", "~"),
                          +New_Path, Revision);
      end if;

      --  For now, have only latin1 keys:
      if not Utf8.Is_Valid_Latin1 (Url.Decode (+New_Path)) then
         raise Malformed_Key with "Only latin1 allowed: " & (+New_Path);
      end if;

      if Kind /= Key.Ksk then
         if not Valid_Length (Crypto) then
            raise Malformed_Key with "Wrong Crypto length: " & Crypto;
         end if;
         Check_Crypto (Crypto);
      end if;

      case Kind is
         when Ksk =>
            if +New_Path = "" then
               raise Malformed_Key with "Empty KSK";
            end if;
            return (Ksk, New_Path);
         when Chk =>
            return (Chk, +(Url.Normalize (+New_Path)), +Crypto);
         when Ssk =>
            declare
               Domain : constant String := Select_Field (+New_Path, 2, '/');
            begin
               if Count (Domain, "-") /= 1 then
                  raise Malformed_Key with "Bad SSK domain: " & (+New_Path);
               elsif not Is_Valid_Number (Tail (Domain, '-')) then
                  raise Malformed_Key with "Bad SSK tail: " & (+New_Path);
               end if;
            end;
            return (Ssk, +(Url.Normalize (+New_Path)), +Crypto);
         when Usk =>
            declare
               Norm_Path : constant String := (Url.Normalize (+New_Path));
               Domain    : constant String := Select_Field (Norm_Path, 2, '/');
            begin
               if Count (Norm_Path, "/") < 2 then
                  raise Malformed_Key with "USK with missing /: " & Norm_Path;
               elsif Domain = "" or Is_Valid_Number (Domain) then
                  raise Malformed_Key with "USK domain invalid: " & Domain;
               elsif Count (Norm_Path, "/") = 2 then -- Just the revision after
                  if Is_Valid_Number (Tail (Norm_Path, '/', 2)) then
                     --  /xxxx/n
                     return (Usk, +(Head (Norm_Path, '/', 2) & "/"),
                             Extract_Revision (Norm_Path), +Crypto);
                  elsif Norm_Path (Norm_Path'Last) = '/' then
                     --  /xxxx/
                     return (Usk, +Norm_Path, Revision, +Crypto);
                  else -- ?? Error
                     raise Malformed_Key with "USK bad path: " & Norm_Path;
                  end if;
               --  #/ > 2
               elsif Select_Field (Norm_Path, 3, '/') = "" then
                  --  /xxxxx//xxxxx
                  return (Usk, +Norm_Path, Revision, +Crypto);
               else
                  --  /xxxxx/n/xxxx
                  if not Is_Valid_Number (Select_Field (Norm_Path, 3, '/')) then
                     raise Malformed_Key with "USK bad revision: " & Norm_Path;
                  end if;
                  return
                    (Usk, +(Head (Norm_Path, '/', 2) & "//" &
                     Tail (Norm_Path, '/', 3)),
                     Extract_Revision (Norm_Path), +Crypto);
               end if;
            end;
      end case;
   end Assemble;

   -----------
   -- Check --
   -----------

   function Check (This : String) return Boolean is
   begin
      Check (This);
      return True;
   exception
      when others =>
         return False;
   end Check;

   -----------
   -- Check --
   -----------

   procedure Check (This : String) is
      K : constant Object := Value (This);
      pragma Unreferenced (K);
   begin
      Check_Crypto (This);
   end Check;

   ------------------
   -- Check_Crypto --
   ------------------

   procedure Check_Crypto (Cryp : Crypto_String) is
   begin
      if Select_Field (Cryp, 1,',')'Length < 43 or else
        Select_Field (Cryp, 1, ',')'Length > 44 or else
        Select_Field (Cryp, 2, ',')'Length < 43 or else
        Select_Field (Cryp, 2, ',')'Length > 44 or else
        Select_Field (Cryp, 3, ',')'Length /= 7
      then
         raise Malformed_Key with "Wrong Crypto parts: " & Cryp;
      end if;
      if not Is_Crypto_Safe (Cryp) then
         raise Malformed_Key with "Unsafe crypto <1010";
      end if;
   end Check_Crypto;

   ----------------------
   -- Extract_Revision --
   ----------------------

   function Extract_Revision (Path : String) return Integer is
   begin
      return Integer'Value (Select_Field (Path, 3, '/'));
   exception
      when others =>
         raise Malformed_Key with "Wrong revision: " & Path;
   end Extract_Revision;

   -----------
   -- Image --
   -----------

   function Image (This : Object) return String is
      Path : constant String := + This.Path;
   begin
      case This.Kind is
         when Usk =>
            return This.Kind'Img & '@' & (+This.Cryp_Usk) & Path_Rev (This);
         when Ksk =>
            return This.Kind'Img & '@' & Path;
         when Chk =>
            if Path'Length > 0 then
               return This.Kind'Img & '@' & (+This.Cryp_Chk) & Path;
            else
               return This.Kind'Img & '@' & (+This.Cryp_Chk);
            end if;
         when Ssk =>
            return This.Kind'Img & '@' & (+This.Cryp_Ssk) & Path;
      end case;
   end Image;

   -----------
   -- Value --
   -----------

   function Value (This : String) return Object is
      Kind   : Kinds;
      --  We make an effort to correct wronged cryptos
      Cryp   : constant String :=
                 Replace
                   (Replace
                      (Tail (Head (This, '/'), '@'), "%7e", "~"),
                    "%7E", "~");
   begin
      begin
         Kind := Kinds'Value (Select_Field (This, 1 , '@'));
      exception
         when others =>
            raise Malformed_Key with "Wrong kind: " & This;
      end;

      --  More checks are performed at Assemble.

      --  Proper value
      case Kind is
         when Usk =>
            return Assemble (Kind,
                             Cryp,
                             '/' & Tail (This),
                             Extract_Revision ('/' & Tail (This)));
         when Chk | Ssk =>
            if Tail (This) = "" and then This (This'Last) /= '/' then
               return Assemble (Kind, Cryp, "", 0);
            else
               return Assemble (Kind, Cryp, '/' & Tail (This, '/'), 0);
            end if;
         when Ksk =>
            return Assemble (Kind, (1 .. Crypto_Length_Min => '.'),
                             Tail (This, '@'), 0);
      end case;
   end Value;

   -----------
   -- Split --
   -----------

   procedure Split (This     :     Object;
                    Kind     : out Kinds;
                    Crypto   : out Ustring;
                    Path     : out Ustring;
                    Revision : out Integer)
   is
   begin
      Crypto   := Null_Ustring;
      Kind     := This.Kind;
      Path     := This.Path;
      Revision := 0;
      case Kind is
         when Ksk =>
            null;
         when Usk =>
            Revision := This.Revi;
            Crypto   := This.Cryp_Usk;
         when Chk =>
            Crypto := This.Cryp_Chk;
         when Ssk =>
            Crypto := This.Cryp_Ssk;
      end case;
   end Split;

   -----------------------
   -- Same_But_Revision --
   -----------------------

   function Same_But_Revision (L, R : Key.Object) return Boolean is
      use Asu;
   begin
      if L.Kind /= Usk or else R.Kind /= Usk then
         raise Constraint_Error with "Only USK have revisions";
      end if;

      return
        L.Cryp_Usk = R.Cryp_Usk and then
        L.Path = R.Path and then
        L.Revi /= R.Revi;
   end Same_But_Revision;

   ----------
   -- Kind --
   ----------

   function Kind (This : Object) return Kinds is
   begin
      return This.Kind;
   end Kind;

   ------------
   -- Crypto --
   ------------

   function Crypto (This : Object) return Crypto_String
   is
   begin
      case This.Kind is
         when Ksk => raise Constraint_Error with "Ksk have no crypto";
         when Usk => return +This.Cryp_Usk;
         when Ssk => return +This.Cryp_Ssk;
         when Chk => return +This.Cryp_Chk;
      end case;
   end Crypto;

   ----------
   -- Path --
   ----------

   function Path (This : Object) return String is
   begin
      return +This.Path;
   end Path;

   --------------
   -- Path_Rev --
   --------------

   function Path_Rev (This : Object) return String is
      Path : constant String := +This.Path;
   begin
      if This.Kind = Usk then
         if Contains (Path, "//") then
            return Head (Path, '/', 2) & '/' & Trim (This.Revi'Img) &
                   '/' & Tail (Path, '/', 3);
            --  return Replace (Path, "//", '/' & Trim (This.Revi'Img) & '/');
            --  Not this way because there may be other //'s there.
         else
            pragma Assert (Path (Path'Last) = '/');
            return Path & Trim (This.Revi'Img);
         end if;
      else
         return Path;
      end if;
   end Path_Rev;

   --------------
   -- Revision --
   --------------

   function Revision (This : Object) return Integer is
   begin
      return This.Revi;
   end Revision;

   ------------------
   -- Set_Revision --
   ------------------

   procedure Set_Revision (This : in out Object; Revision : Integer) is
   begin
      This.Revi := Revision;
   end Set_Revision;

   ------------------
   -- Valid_Length --
   ------------------

   function Valid_Length (Crypto : Crypto_String) return Boolean is
   begin
      return Crypto'Length >= Crypto_Length_Min and then Crypto'Length <= Crypto_Length_Max;
   end Valid_Length;

   ---------------------
   -- Is_Valid_Number --
   ---------------------

   function Is_Valid_Number (X : String) return Boolean is
   begin
      if Trim (Integer'Value (X)'Img) /= X then
         return False;
         --  raise Constraint_Error with "Invalid revision: " & X;
      end if;

      return True;
   exception
      when others =>
         --  Log ("Is_Valid_Number: " & Report (E), Warning);
         return False;
   end Is_Valid_Number;

   --------------------
   -- Is_Crypto_Safe --
   --------------------

   function Is_Crypto_Safe (Crypto : Key.Crypto_String) return Boolean is
   begin
      return not (
      Contains (Crypto, "AQABAAE") or else
      Contains (Crypto, "AAEA--8") or else
      Contains (Crypto, "AAEAAAA") or else
      Contains (Crypto, "AAEC--8") or else
      Contains (Crypto, "AAECAAA")
                  );
   end Is_Crypto_Safe;

   ----------------
   -- Ssk_To_Usk --
   ----------------

   function Ssk_To_Usk (This : Object) return Object is
      New_Path : String := Path_Rev (This);
   begin
      New_Path (Pos (New_Path, "-")) := '/';
      return Assemble (Usk,
                       Crypto (This),
                       New_Path,
                       0); -- The revision is taken from path
   end Ssk_To_Usk;

   ----------------
   -- Usk_To_Ssk --
   ----------------

   function Usk_To_Ssk (This : Object) return Object is
      New_Path : String := Path_Rev (This);
   begin
      New_Path (Pos (New_Path, "/", 2)) := '-';
      return Assemble (Ssk,
                       Crypto (This),
                       New_Path);
   end Usk_To_Ssk;

   ------------
   -- Domain --
   ------------

   function Domain (This : Object) return String is
   begin
      case This.Kind is
         when Usk =>
            return Select_Field (Path (This), 2, '/');
         when Ssk =>
            return Head (Select_Field (Path (This), 2, '/'), '-');
         when Chk | Ksk =>
            raise Constraint_Error with "No domain of key type " & This.Kind'Img;
      end case;
   end Domain;

   ----------
   -- File --
   ----------

   function File (This : Object) return String is
      Path : constant String := Key.Path (This);
   begin
      case This.Kind is
         when Ksk | Chk =>
            return String_Head_Reverse (Path);
         when Usk =>
            if Count (Path, "/") >= 3 then
               return String_Head_Reverse (Path);
            else
               return "";
            end if;
         when Ssk =>
            if Count (Path, "/") >= 2 then
               return String_Head_Reverse (Path);
            else
               return "";
            end if;
      end case;
   end File;

   -------------
   -- Is_Bare --
   -------------

   function Is_Bare (This : Object) return Boolean is
      Path : constant String := Path_Rev (This);
   begin
      case This.Kind is
         when Ksk | Chk =>
            return Path = "";
         when Usk =>
            return Path (Path'Last) /= '/' and Count (Path, "/") = 2;
         when Ssk =>
            return Path (Path'Last) /= '/' and Count (Path, "/") = 1;
      end case;
   end Is_Bare;

   -----------------
   -- Parent_Path --
   -----------------

   function Parent_Path (This : Object) return String is
      Path : constant String := Key.Path_Rev (This);
   begin
      if Path = "" then
         return "/";
      elsif Path (Path'Last) = '/' then
         return Path;
--        elsif Is_Bare (This) then
--           return Path & '/';
      else
         return String_Tail_Reverse (Path) & '/';
      end if;
   end Parent_Path;

end Adafn.Key;
