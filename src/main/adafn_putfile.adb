with Adafn.Callbacks;
with Adafn.Identifier;
with Adafn.Inserts;
with Adafn.Key;
with Adafn.Node;
with Adafn.Options;
with Adafn.Source;
with Adafn.Source.Vectors;
use Adafn;

with Agpl.Command_Line; use Agpl.Command_Line;
with Agpl.Trace; use Agpl.Trace;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Ada.Calendar; use Ada.Calendar;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_Io; use Ada.Text_Io;

with Gnat.Os_Lib;

procedure Adafn_Putfile is
   Node   : Adafn.Node.Object;
   Id     : Identifier.Object;
   Epoch  : constant Time := Time_Of (1976, 9, 6);

   Use_Key : Boolean;
   Opts    : Options.For_Insert;
begin
   if Argument_Count = 0 then
      Put_Line ("Arguments: --key <key> --name <name> --source <data> [-v] [-q (quiet)] [--wait] [--uncompressed] [--source-kind <direct|disk|redirect>");
      Gnat.Os_Lib.Os_Exit (1);
   end if;

   if Exists ("-v") then
      Enable_Section (Adafn.Log_Section);
      Set_Level (Debug);
   end if;

   Node.Connect ("AdaFN" & Duration'Image (Clock - Epoch));

   delay 0.2;

   Use_Key := Exists ("--key");
   Opts.Compress    := not Exists ("--uncompressed");
   Opts.Verbose     := 513;
   Opts.Priority    := 0;
   if Exists ("--wait") then
      Opts.Persistence  := Connection;
      Opts.Global_Queue := False;
   end if;

   declare
      use type Key.Kinds;
      Src : Source.Object (Source.Kinds'Value (Get_Option ("--source-kind")));
   begin
      Src.Name := +Get_Option ("--name");
      case Src.Kind is
         when Source.Direct =>
            Src.Data := +Get_Option ("--source");
         when Source.Disk | Source.Directfromdisk =>
            Src.Path := +Get_Option ("--source");
         when Source.Redirect =>
            Src.Uri  := Key.Value (Get_Option ("--source"));
      end case;

      if Use_Key then
         declare
            Uri : constant Key.Object := Key.Value (Get_Option ("--key"));
         begin
            --  Note the OR TRUE
            --  I don't remember why I was using Complex for single files
            --  I guess it is because Putfile doesn't guesses mime types,
            --  while putdir does (it's the node's fault not to be consistent)
            if Key.Kind (Uri) = Key.Ksk or True then
               Node.Insert_File (Src,
                                 Inserts.Create (Uri),
                                 Id, Cb => Callbacks.Cb'Access, Opts => Opts);
            else
               Opts.Default_File := Src.Name;
               Node.Insert_Complex
                 (Uri,
                  Source.Vectors.To_Vector (Src, 1),
                  Id, Cb => Callbacks.Cb'Access, Opts => Opts);
            end if;
         end;
      else
         Node.Insert_File (Src, Inserts.Create (Key.Chk),
                           Id, Cb => Callbacks.Cb'Access, Opts => Opts);
      end if;
   end;

   while Node.Is_Connected loop
      delay 0.1;
   end loop;

   Gnat.Os_Lib.Os_Exit (1);

exception
   when E : others =>
      Put_Line (Standard_Error, Agpl.Trace.Report (E));
      Gnat.Os_Lib.Os_Exit (1);
end Adafn_Putfile;
