 

--  Facilities for saving/loading from a file in a way that it doesn't
--  overwrite the previous version.
--  When opening for writing, a temporary is used. After completion, the
--  original file is removed and the temporary renamed.
--  When opening for reading, regular filename is tried. If missing, a check
--  for a temporary is made, and if it exists, it is renamed to the regular
--  name and opened. If neither regular and temporary exists, error.

with Ada.Streams.Stream_IO;
use  Ada.Streams;
use  Ada;

package Agpl.Safe_file is

   --  pragma Elaborate_Body;

   File_Not_Found : exception;

   ------------------------------------------------------------------------
   -- Exists_for_reading                                                 --
   ------------------------------------------------------------------------
   function Exists_for_reading (Name : in String) return Boolean;

   ------------------------------------------------------------------------
   -- Get_Real_Name                                                      --
   ------------------------------------------------------------------------
   --  Gets the real name found (i.e. the supplied or the backup one
   --  May raise File_Not_Found
   function Get_Real_Name (Name : in String) return String;

   ------------------------------------------------------------------------
   -- Open                                                               --
   ------------------------------------------------------------------------
   procedure Open (
      File : in out Stream_IO.File_type;
      Mode : in     Stream_IO.File_mode;
      Name : in     String := "";
      Form : in     String := "");

   ------------------------------------------------------------------------
   -- Close                                                              --
   ------------------------------------------------------------------------
   procedure Close (File : in out Stream_IO.File_type);

end Agpl.Safe_file;
