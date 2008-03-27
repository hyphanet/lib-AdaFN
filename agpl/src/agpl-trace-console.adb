with Agpl.Text_Io;

package body Agpl.Trace.Console is

   ---------
   -- Log --
   ---------

   procedure Log
     (This    : in out Object;
      Text    : in String;
      Level   : in Levels;
      Section : in String := "")
   is
   begin
      Agpl.Text_Io.Put_Line (This.Decorate (Text, Level, Section));
   end Log;

end Agpl.Trace.Console;
