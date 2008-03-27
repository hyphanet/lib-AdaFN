with Agpl.Containers.String_String_Maps;
with Agpl.Strings;
with Agpl.Strings.Fields;
with Agpl.Trace; use Agpl.Trace;

with Ada.Text_Io;

package body Adafn.Metadata is

   Ext_To_Mime : Agpl.Containers.String_String_Maps.Map;

   function "<" (L, R : Mime_Type) return Boolean is
   begin
      return +L.Descr < +R.Descr;
   end "<";

   ------------
   -- Create --
   ------------

   function Create (From : String) return Object is
      use Agpl.Strings; use Fields;
   begin
      return (Mime     => Value (Trim (Select_Field (From, 1, ';'))),
              Encoding => +Trim (L (Select_Field (From, 2, ';'))),
              Size     => 0);
   end Create;

   -----------
   -- Image --
   -----------

   function Image (This : Mime_Type) return String is
   begin
      return + This.Descr;
   end Image;

   -----------
   -- Value --
   -----------

   function Value (This : String) return Mime_Type is
      use Agpl.Strings;
   begin
      return (Descr => + L (This));
   end Value;

   --------------------------
   -- Guess_From_Extension --
   --------------------------

   function Guess_From_Extension (Ext : String) return Mime_Type is
      use Agpl.Strings;
   begin
      if Ext_To_Mime.Contains (L (Ext)) then
         return (Descr => +Ext_To_Mime.Element (L (Ext)));
      else
         return Application_Octet_Stream;
      end if;
   end Guess_From_Extension;

   -------------------------
   -- Guess_From_Filename --
   -------------------------

   function Guess_From_Filename (Name : String) return Mime_Type is
      use Agpl.Strings.Fields;
   begin
      return Guess_From_Extension (String_Head_Reverse (Name, '.'));
   end Guess_From_Filename;

   --------------------
   -- Guess_From_Key --
   --------------------

   function Guess_From_Key (Uri : Key.Object) return Mime_Type is
      use Agpl.Strings; use Fields;
      use type Key.Kinds;
      Mime : Mime_Type :=
               Guess_From_Extension
                 (String_Head_Reverse (Key.Image  (Uri), '.'));
   begin
      if Mime = Application_Octet_Stream then
         case Key.Kind (Uri) is
            when Key.Usk =>
               Mime := Guess_From_Extension
                 (String_Head_Reverse (Key.Domain (Uri), '.'));
               if Mime /= Application_Octet_Stream then
                  null;
                  --  Log ("Gessed from Domain: " & Key.Domain (Uri) & ", " & Image (Mime), always);
               end if;
            when others =>
               null;
         end case;
      end if;

      if Mime = Application_Octet_Stream then
         case Key.Kind (Uri) is
            when Key.Usk =>
               --  USK without file are considered good candidates!
               if Key.File (Uri) = "" then
                  Mime := Application_X_Presumed_Html;
                  --  Log ("Presuming HTML for " & Key.Path_Rev (Uri), Always);
               end if;
            when others  =>
               null;
         end case;
      end if;

      if Mime = Application_Octet_Stream and then key.kind (uri) = Key.Usk then
         null;
         --  Log ("Failed to guess for " & Key.Path_Rev (Uri) & " :: " & Key.File (Uri), Always);
      end if;

      return Mime;
   end Guess_From_Key;

   ----------------
   -- Load_Types --
   ----------------

   procedure Load_Types is
      use Agpl.Strings; use Fields;
      use Ada.Text_Io;
      F : File_Type;
   begin
      Open (F, In_File, "/etc/mime.types");
      while not End_Of_File (F) loop
         declare
            Str : constant String := Crunch (Trim (Untab (Get_Line (F))));
         begin
            if Str'Length > 0 and then Str (Str'First) /= '#' then
               for I in 2 .. Natural'Last loop
                  exit when Select_Field (Str, I) = "";
                  Ext_To_Mime.Include (L (Select_Field (Str, I)),
                                       L (Select_Field (Str, 1)));
               end loop;
            end if;
         end;
      end loop;
      Close (F);

      --  Add hardcoded types who have handlers:
      Ext_To_Mime.Include ("xml",  Application_Xml.Image);
      Ext_To_Mime.Include ("html", Text_Html.Image);
      Ext_To_Mime.Include ("htm",  Text_Html.Image);
      Ext_To_Mime.Include ("frdx", Application_X_Freenet_Index.Image);

      Log ("Mime types loaded:" & Ext_To_Mime.Length'Img, Informative);
   end Load_Types;

begin
   Load_Types;
exception
   when E : others =>
      Log ("Adafn.Mimetypes: Couldn't initialize: " & Report (E), Error);
      raise;
end Adafn.Metadata;
