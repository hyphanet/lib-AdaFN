 

with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

package Agpl.Filesystem is

--   pragma Preelaborate;

   --  Returns the same string with a Folder_Separator added if it is missing.
   function Ensure_Slash (This : in String; Separator : in Character := '/')
      return String;
   pragma Inline (Ensure_Slash);

   function Replace_Extension (This : in String; New_Ext : in String)
                               return    String;
   --  Replace the extension by a new one

   function Read_File (Name : String) return Ustring;
   --  Read a full file as a string!

end Agpl.Filesystem;
