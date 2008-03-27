with Adafn.Callbacks;
with Adafn.Identifier;
with Adafn.Key;
with Adafn.Node;
with Adafn.Options;
use Adafn;

with Agpl.Command_Line; use Agpl.Command_Line;
with Agpl.Trace; use Agpl.Trace;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Gnat.Os_Lib;

with Ada.Text_Io; use Ada.Text_Io;

procedure Adafn_Putdir is
   Node   : Adafn.Node.Object;
   Id     : Identifier.Object;

   Opts    : Options.For_Insert;
begin
   if Exists ("-v") then
      Enable_Section (Adafn.Log_Section);
      Set_Level (Debug);
   end if;

   Node.Connect;

   Opts.Compress     := not Exists ("--uncompressed");
   Opts.Verbose      := 513;
   Opts.Priority     := 0;
   Opts.Default_File := +Get_Option ("--default-file");
   Opts.Global_Queue := not Exists ("--local-queue");
   if Opts.Global_Queue then
      Opts.Persistence := Forever;
   else
      Opts.Persistence := Connection;
   end if;
   if Exists ("--direct") then
      Opts.Preferred_Send := Direct;
   else
      Opts.Preferred_Send := Disk;
   end if;

   Node.Insert_Dir
     (Key.Value (Get_Option ("--key")),
      Get_Option ("--dir"),
      Id,
      Cb => Callbacks.Cb'Access,
      Opts => Opts);

   while Node.Is_Connected loop
      delay 0.1;
   end loop;

   Gnat.Os_Lib.Os_Exit (1);

exception
   when E : others =>
      Put_Line (Standard_Error, Agpl.Trace.Report (E));
end Adafn_Putdir;
