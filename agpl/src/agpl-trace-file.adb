package body Agpl.Trace.File is

   use Ada.Text_Io;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Destructor_Type) is
   begin
      if This.Parent.Opened then
         Close (This.Parent.File);
      end if;
   end Finalize;

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
      if not This.Opened then
         Set_File (This);
         pragma Assert (This.Opened);
      end if;

      Put_Line (This.File, This.Decorate (Text, Level, Section));
      Flush (This.File);
   end Log;

   --------------
   -- Set_File --
   --------------

   procedure Set_File
     (This : in out Object;
      Name : in     String := Command_Line.Program_Name & "." &
                              Calendar.Format.Datestamp (Separator =>'.') &"."&
                              Calendar.Format.Timestamp & ".log";
      Mode : in     Modes  := Append)
   is
   begin
      case Mode is
         when Append =>
            Create (This.File, Append_File, Name);
         when Reset =>
            Create (This.File, Out_File, Name);
      end case;
      This.Opened := True;
   end Set_File;

end Agpl.Trace.File;
