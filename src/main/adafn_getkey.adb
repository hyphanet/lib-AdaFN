with Adafn.Errors;
with Adafn.Key;
with Adafn.Node;
with Adafn.Options;
with Adafn.Result;
with Adafn.Transaction;
use Adafn;

with Agpl.Command_Line; use Agpl.Command_Line;
with Agpl.Trace; use Agpl.Trace;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Gnat.Os_Lib;

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_Io; use Ada.Text_Io;

procedure Adafn_Getkey is
   Node   : Adafn.Node.Object;
   Data   : Result.Object;
   Result : Transaction.Blocking_States;

   Err    : Errors.For_Request;
   Opts   : Options.For_Request := (Verbose  => Options.Verbose_Progress,
                                    Priority => Priorities'First,
                                    Timeout  => Duration'Last,
                                    others   => <>);
   Name   : Ustring := +"AdaFN.Getkey";
begin
   if Argument_Count = 0 then
      Put_Line ("Arguments: -k <key> [-v] [-f] [--max-size <n>] [--retries <n>] [--name <name>] [--global]");
      Gnat.Os_Lib.Os_Exit (1);
   end if;

   if Exists ("-v") then
      Opts.Verbose := Options.Verbose_All;
      Enable_Section (Adafn.Log_Section);
      Set_Level (Debug);
   end if;

   if Exists ("-f") then
      Opts.Follow_Redirects := Natural'Last;
   else
      Opts.Follow_Redirects := 0;
   end if;

   if Exists ("--retries") then
      Opts.Max_Retries := Natural'Value (Get_Option ("--retries"));
   else
      Opts.Max_Retries := 0;
   end if;

   if Exists ("--max-size") then
      Opts.Max_Size := Natural'Value (Get_Option ("--max-size"));
   end if;

   if Exists ("--name") then
      Name := + Get_Option ("--name");
   end if;

   if Exists ("--global") then
      Opts.Global_Queue := True;
      Opts.Persistence  := Forever;
   end if;

   Node.Connect (+Name);

   Node.Get_Key
     (Key.Value (Get_Option ("-k")),
      Data,
      Result,
      Err,
      Opts => Opts);

   declare
      use Transaction;
   begin
      if Result = Completed then
         Put_Line ("AT :" & Key.Image (Data.Uri));
         Put_Line (+Data.Data);
         Node.Shutdown;
         Gnat.Os_Lib.Os_Exit (0);
      else
         Put_Line (Standard_Error, "Failed (Fatal=" & Err.Fatal'Img & ")");
         Node.Shutdown;
         Gnat.Os_Lib.Os_Exit (1);
      end if;
   end;
exception
   when E : others =>
      Put_Line (Standard_Error, Agpl.Trace.Report (E));
end Adafn_Getkey;
