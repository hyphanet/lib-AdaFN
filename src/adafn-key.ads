with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

package Adafn.Key is

   Malformed_Key : exception;

   type Kinds is (Ssk, Usk, Chk, Ksk);

   Crypto_Length_Min : constant Positive := 95;
   Crypto_Length_Max : constant Positive := 97;

   subtype Crypto_String is String;

   type Object (Kind : Kinds := Chk) is private;

   Null_Object : constant Object;

   function Assemble (Kind     : Kinds;
                      Crypto   : Crypto_String;
                      Path     : String;
                      Revision : Integer := 0) return Object;
   --  Compose a key from its components.
   --  Won't accept malformed crypto
   --  Revision is only needed for USKs
   --  USK path can contain // or /n/ for revision, both are accepted
   --  For non-container USKs, it may end in / or /n, both accepted
   --  The valid revision is the one given in Path if present, or Revision c.c.
   --  Paths start *with* /, if existing (may be empty for bare CHKs)

   function Ssk_To_Usk (This : Object) return Object;
   --  Get a USK for the given SSK

   function Usk_To_Ssk (This : Object) return Object;
   --  Get a SSK for the given USK

   function "<" (L, R : Object) return Boolean;

   function Check (This : String) return Boolean;
   --  Say if a key is valid.

   procedure Check (This : String);
   --  As previous, but will raise Malformed_Key

   procedure Check_Crypto (Cryp : Crypto_String);
   --  Ensures Cryp is well formed

   function Is_Crypto_Safe (Crypto : Crypto_String) return Boolean;
   pragma Inline (Is_Crypto_Safe);
   --  Checks not <1010 crypto

   function Clip (S : String) return String;
   --  Remove a trailing '/', if there is

   function Image (This : Object) return String;
   pragma Inline (Image);

   function Value (This : String) return Object;
   pragma Inline (Value);

   procedure Split (This     :     Object;
                    Kind     : out Kinds;
                    Crypto   : out Ustring;
                    Path     : out Ustring;
                    Revision : out Integer);
   --  Get a key object and give its components.
   --  Path is stripped of revision for USKs
   --  Revision is meaningful for USKs only (uninitialized otherwise).

   function Same_But_Revision (L, R : Key.Object) return Boolean;
   --  Compares two keys. They're equal if only difference is the revision.

   function Kind (This : Object) return Kinds;
   function Crypto (This : Object) return Crypto_String;
   function Path (This : Object) return String;
   --  Without Rev for USKs
   function Path_Rev (This : Object) return String;
   --  With Rev for USKs
   function Revision (This : Object) return Integer;

   procedure Set_Revision (This : in out Object; Revision : Integer);

   function Domain (This : Object) return String;
   --  Part before / or - in USK/SSK
   function File (This : Object) return String;
   --  Last part after revision or -n in USK/SSK (without separator slash) or
   --  last part for ksk/chk

   function Is_Bare (This : Object) return Boolean;
   --  True for USK/SSK/CHK without trailing / nor file part:
   --  usk@blah/0

   function Parent_Path (This : Object) return String;
   --  Parent path of the file. If there's no file, but ends in '/',
   --  identity.
   --  Parent always end in '/', even for void paths...

private

   type Object (Kind : Kinds := Chk) is record
      Path : Ustring; -- Path includes the first /
      case Kind is
         when Ksk    =>
            null;
         when Usk =>
            Revi     : Integer := 0;
            Cryp_Usk : Ustring;
         when Chk =>
            Cryp_Chk : Ustring;
         when Ssk =>
            Cryp_Ssk : Ustring;
      end case;
   end record;

   Null_Object : constant Object := (Ksk, Null_Ustring);

   function Extract_Revision (Path : String) return Integer;

   function Valid_Length (Crypto : Crypto_String) return Boolean;

   function Is_Valid_Number (X : String) return Boolean;

end Adafn.Key;
