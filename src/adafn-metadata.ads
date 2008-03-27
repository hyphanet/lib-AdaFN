with Adafn.Key;

with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

package Adafn.Metadata is

   pragma Elaborate_Body; -- To load mime types

   type Mime_Type is tagged private;

   function "<" (L, R : Mime_Type) return Boolean;

   function Image (This : Mime_Type) return String;

   function Value (This : String) return Mime_Type;

   function Guess_From_Extension (Ext : String) return Mime_Type;

   function Guess_From_Filename (Name : String) return Mime_Type;

   function Guess_From_Key (Uri : Key.Object) return Mime_Type;


   --  Some explicitly named mime types used across the spider
   Application_Octet_Stream    : constant Mime_Type; -- Default for unknown types
   Text_Plain                  : constant Mime_Type;
   Text_Html                   : constant Mime_Type;
   Application_X_Freenet_Index : constant Mime_Type;
   Application_Xml             : constant Mime_Type;
   Application_X_Presumed_Html : constant Mime_Type;

   type Object is record
      Mime     : Mime_Type := Application_Octet_Stream;
      Encoding : Ustring;
      Size     : Key_Size  := 0;
   end record;

   function Create (From : String) return Object;
   --  "text/plain;utf-8"

private

   type Mime_Type is tagged record
      Descr : Ustring;
   end record;

   Application_Octet_Stream    : constant Mime_Type := (Descr => + "application/octet-stream");
   Text_Plain                  : constant Mime_Type := (Descr => + "text/plain");
   Text_Html                   : constant Mime_Type := (Descr => + "text/html");
   Application_X_Freenet_Index : constant Mime_Type := (Descr => + "application/x-freenet-index");
   Application_Xml             : constant Mime_Type := (Descr => + "application/xml");
   Application_X_Presumed_Html : constant Mime_Type := (Descr => + "application/x-presumed-html");

end Adafn.Metadata;
