with Adafn.Identifier;
with Adafn.Key;
with Adafn.Node;
with Adafn.Options;
with Adafn.Result;
with Adafn.Transaction;
use Adafn;

with Agpl.Command_Line; use Agpl.Command_Line;
with Agpl.Trace;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Ada.Text_Io; use Ada.Text_Io;

procedure Adafn_Test is

   Node   : Adafn.Node.Object;
   Data   : Result.Object;
   Result : Transaction.Blocking_States;
begin
   if Exists ("-v") then
      Agpl.Trace.Set_Level (Agpl.Trace.Debug);
      Agpl.Trace.Enable_Section (Adafn.Log_Section);
   end if;

   Put_Line ("Connecting...");
   Node.Connect;
   Put_Line ("Ok.");

   --  Test blocking key fetch...
   Put_Line ("Requesting KSK@gpl.txt (blocking)...");
   Node.Get_Key
     (Key.Value ("KSK@gpl.txt"),
      Data,
      Result,
      Opts => Options.For_Request'(Verbose      => Options.Verbose_Progress,
                                   Timeout      => Duration'Last,
                                   others       => <>));
   Put_Line ("Result: " & Result'Img);
   Put_Line ("Data:" & Asu.Length (Data.Data)'Img & " bytes");

   delay 1.0;

   --  Test non-blocking key fetch
   declare
      Id   : Identifier.Object;
      Done : Boolean := False;
      procedure Cb (Tran :     Transaction.Object;
                    Act  : out Transaction.Actions)
      is
         pragma Unreferenced (Act);
         use Transaction;
      begin
         Put_Line ("Callback status: " & Status (Tran)'Img);
         Put_Line ("Data:" &
                   Asu.Length (Transaction.Data (Tran).Data)'Img & " bytes");
         Done := True;
      end Cb;
   begin
      Put_Line ("Requesting KSK@gpl.txt (non-blocking)...");
      Node.Get_Key
        (Key.Value ("KSK@gpl.txt"),
         Id => Id,
         Cb => Cb'Unrestricted_Access,
         Opts => Options.For_Request'(Verbose      => Options.Verbose_Progress,
                                      Timeout      => Duration'Last,
                                      others       => <>));
      while not Done loop
         delay 0.1;
      end loop;
      Put_Line ("Request asynchronously found.");
      Put_Line ("Data:" &
                Asu.Length (Node.Get_Data (Id).Data)'Img & " bytes");
      Node.Remove_Transaction (Id);
   end;

   Node.Shutdown;
exception
   when E : others =>
      Put_Line (Agpl.Trace.Report (E));
end Adafn_Test;
