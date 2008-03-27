 

package Agpl.Command_line is

   pragma Elaborate_Body;

   Option_Not_Supplied : exception;

   ------------------------------------------------------------------------
   --  Exists                                                            --
   ------------------------------------------------------------------------
   --  Ensure an option has been supplied
   function Exists (Option : in String) return Boolean;

   ------------------------------------------------------------------------
   --  Get_Option                                                        --
   ------------------------------------------------------------------------
   --  Returns the string after the given option (f.e: "-f <file>" will return
   --   <file>
   --  May raise Option_Not_Supplied
   function Get_Option (Option : in String) return String;

   ------------------------------------------------------------------------
   -- Program_path                                                       --
   ------------------------------------------------------------------------
   --   Returns the path to the executable (with trailing /, without the file).
   function Program_path return String;

   ------------------------------------------------------------------------
   -- Program_name                                                       --
   ------------------------------------------------------------------------
   --  Returns the executable's name, without dot extension.
   --  I.e., the same in every platform.
   function Program_name return String;

   ------------------------
   -- Wait_For_Keystroke --
   ------------------------
   procedure Wait_For_Keystroke;
   --  Waits until some key is pressed in the command line.

end Agpl.Command_line;
