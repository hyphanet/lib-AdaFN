 

--  Just implements sections but still doesn't trace.

with Agpl.Calendar.Format;
with Agpl.Command_Line;
with Agpl.Trace.Root;

private with Ada.Finalization;
with Ada.Text_Io;

package Agpl.Trace.File is

   pragma Elaborate_Body;

   type Object is new Root.Object with private;
   type Object_Access is access all Object'Class;

   type Modes is (Append, Reset);

   overriding
   procedure Log (This    : in out Object;
                  Text    : in String;
                  Level   : in Levels;
                  Section : in String := "");

   not overriding
   procedure Set_File
     (This : in out Object;
      Name : in     String := Command_Line.Program_Name & "." &
                              Calendar.Format.Datestamp (Separator =>'.') &"."&
                              Calendar.Format.Timestamp & ".log";
      Mode : in     Modes  := Append);

private

   type Destructor_Type (Parent : access Object) is new
     Ada.Finalization.Limited_Controlled with null record;

   type Object is new Root.Object with record
      File       : Ada.Text_Io.File_Type;
      Opened     : Boolean := False;

      Destructor : Destructor_Type (Object'Access);
   end record;

   procedure Finalize (This : in out Destructor_Type);

end Agpl.Trace.File;
