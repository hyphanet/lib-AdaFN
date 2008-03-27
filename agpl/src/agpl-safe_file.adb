 

--  Facilities for saving/loading from a file in a way that it doesn't
--  overwrite the previous version.
--  When opening for writing, a temporary is used. After completion, the
--  original file is removed and the temporary renamed.
--  When opening for reading, regular filename is tried. If missing, a check
--  for a temporary is made, and if it exists, it is renamed to the regular
--  name and opened. If neither regular and temporary exists, error.

with Agpl.Exceptions;

with Ada.Streams.Stream_IO;
use  Ada.Streams.Stream_IO;

with Gnat.Os_lib;
use  Gnat;

package body Agpl.Safe_file is

   ------------------------------------------------------------------------
   -- Exists_for_reading                                                 --
   ------------------------------------------------------------------------
   function Exists_for_reading (Name : in String) return Boolean is
   begin
      return Os_lib.Is_regular_file (Name) or else
             Os_lib.Is_regular_file (Name & ".tmp");
   end Exists_for_reading;

   ------------------------------------------------------------------------
   -- Get_Real_Name                                                      --
   ------------------------------------------------------------------------
   --  Gets the real name found (i.e. the supplied or the backup one
   function Get_Real_Name (Name : in String) return String is
   begin
      if Os_Lib.Is_Regular_File (Name) then
         return Name;
      elsif Os_Lib.Is_Regular_File (Name & ".tmp") then
         return Name & ".tmp";
      else
         raise File_Not_Found;
      end if;
   end Get_Real_Name;

   ------------------------------------------------------------------------
   -- Open                                                               --
   ------------------------------------------------------------------------
   --  Accepts In_file and Out_file
   procedure Open (
      File : in out Stream_IO.File_type;
      Mode : in     Stream_IO.File_mode;
      Name : in     String := "";
      Form : in     String := "")
   is
      Success : Boolean;
   begin
      case Mode is
         when In_file =>
            if Os_lib.Is_regular_file (Name & ".tmp") and
               not Os_lib.Is_regular_file (Name)
            then
               Os_lib.Rename_file (Name & ".tmp", Name, Success);
               if not Success then
                  raise Storage_Error;
               end if;
            end if;
            Stream_io.Open (File, Mode, Name, Form);
         when Out_file =>
            Stream_io.Create (File, Mode, Name & ".tmp", Form);
         when Append_file =>
            raise Exceptions.Unimplemented;
      end case;
   end Open;

   ------------------------------------------------------------------------
   -- Close                                                              --
   ------------------------------------------------------------------------
   procedure Close (File : in out Stream_IO.File_type) is
      Filename : constant String := Stream_IO.Name (File);
      Success  : Boolean;
   begin
      case Mode (File) is
         when In_file =>
            Stream_IO.Close (File);
         when Out_file =>
            Stream_IO.Close (File);
            --  Delete previous, if exists:
            if Os_lib.Is_regular_file (
               Filename (Filename'First .. Filename'Last - 4))
            then
               Os_lib.Delete_file (
                  Filename (Filename'First .. Filename'Last - 4), Success);
               if not Success then
                  raise Storage_Error;
               end if;
            end if;
            --  Rename file:
            Os_lib.Rename_file (
               Filename,
               Filename (Filename'First .. Filename'Last - 4),
               Success);
            if not Success then
               raise Storage_Error;
            end if;
         when Append_file =>
            raise Exceptions.Unimplemented;
      end case;
   end Close;

end Agpl.Safe_file;
