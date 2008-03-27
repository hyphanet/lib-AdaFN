 

with Ada.Command_line;
with Ada.Text_Io;

with Gnat.Directory_operations;
with Gnat.Os_lib;

package body Agpl.Command_line is

   package Cl  renames Ada.Command_line;
   package Dop renames Gnat.Directory_operations;
   package Ol  renames Gnat.Os_lib;

   ------------------------------------------------------------------------
   --  Exists                                                            --
   ------------------------------------------------------------------------
   --  Ensure an option has been supplied
   function Exists (Option : in String) return Boolean is
   begin
      declare
         Dummy : constant String := Get_Option (Option);
         pragma Unreferenced (Dummy);
      begin
         --  If here, it means the option exists
         return True;
      end;
   exception
      when Option_Not_Supplied =>
         return False;
   end Exists;

   ------------------------------------------------------------------------
   --  Get_Option                                                        --
   ------------------------------------------------------------------------
   --  Returns the string after the given option (f.e: "-f <file>" will return
   --   <file>
   --  May raise Option_Not_Supplied
   function Get_Option (Option : in String) return String is
   begin
      for I in 1 .. Cl.Argument_Count loop
         if Cl.Argument (I) = Option then
            if I < Cl.Argument_Count then
               return Cl.Argument (I + 1);
            else
               return "";
            end if;
         end if;
      end loop;

      raise Option_Not_Supplied with "Missing option: " & Option;
   end Get_Option;

   ------------------------------------------------------------------------
   -- Program_path                                                       --
   ------------------------------------------------------------------------
   --  Returns the path to the executable (with trailing /, without the file).
   function Program_path return String is
   begin
      return Dop.Dir_name (Ol.Normalize_pathname (Cl.Command_name));
   end Program_path;

   ------------------------------------------------------------------------
   -- Program_name                                                       --
   ------------------------------------------------------------------------
   --  Returns the executable's name, without dot extension.
   --  I.e., the same in every platform.
   function Program_name return String is
   begin
      return Dop.Base_name (
         Cl.Command_name,
         Dop.File_extension (Cl.Command_name));
   end Program_name;

   ------------------------
   -- Wait_For_Keystroke --
   ------------------------

   procedure Wait_For_Keystroke is
      C : Character;
      use Ada.Text_IO;
   begin
      Get_Immediate (C);
   end Wait_For_Keystroke;

end Agpl.Command_line;
