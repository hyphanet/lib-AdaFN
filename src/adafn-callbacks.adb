with Adafn.Message;

with Agpl.Average_Queue_Float;
with Agpl.Chronos;
with Agpl.Command_Line; use Agpl.Command_Line;
with Agpl.If_Function;
with Agpl.Strings; use Agpl.Strings;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Text_Io; use Ada.Text_Io;

with Gnat.Os_Lib;

package body Adafn.Callbacks is

   function Iif is new Agpl.If_Function (String);

   ------------
   -- Silent --
   ------------

   function Silent return Boolean is
   begin
      return Exists ("-q");
   end Silent;

   --------
   -- Cb --
   --------
   Toter : Agpl.Chronos.Object;
   Timer : Agpl.Chronos.Object;
   Avg   : Agpl.Average_Queue_Float.Object (10);
   procedure Cb (This    :     Transaction.Object;
                 Action  : out Transaction.Actions)
   is
      use Adafn.Transaction;
      Msg  : constant Message.Object'Class := This.Last_Answer;
      Name : Ustring;

      function Percent (Msg : Message.Object'Class) return Float is
      begin
         return
            Float'Value (Msg.Value ("Succeeded", "0")) * 100.0 /
            Float'Value (Msg.Value ("Required", "0"));
      exception
         when others =>
            return 0.0;
      end Percent;

      function Percent_Str (Msg : Message.Object'Class) return String is
      begin
         return To_String (Percent (Msg)) & "%";
      exception
         when others =>
            return "?%";
      end Percent_Str;
   begin
      if Exists ("--name") then
         Name := +Get_Option ("--name");
      elsif Exists ("--default-file") then
         Name := +Get_Option ("--default-file");
      end if;

      Action := Nothing;
      case This.Last_Answer.Kind is
         when Message.Protocolerror =>
            Put_Line (Standard_Error, "Failed: ProtocolError");
            Gnat.Os_Lib.Os_Exit (1);
         when Message.Persistentput | Message.Persistentputdir =>
            if not Exists ("--wait") then
               Gnat.Os_Lib.Os_Exit (0);
            end if;
         when Message.Putfailed =>
            Put_Line (Standard_Error, "Failed: PutFailed");
            Gnat.Os_Lib.Os_Exit (1);
         when Message.Putsuccessful | Message.Urigenerated =>
            if not Silent then
               Put_Line (Lpad (+Name, 14) & ": " & Msg.Kind_Image (1 .. 3) &
                         ": " & Msg.Value ("URI"));
            end if;
            Gnat.Os_Lib.Os_Exit (0);
         when Message.Simpleprogress =>
            if not Silent then
               declare
                  Succ  : constant Natural :=
                           Natural'Value (Msg.Value ("Succeeded", "0"));
                  Total : constant Natural := Natural'Max
                    (Succ, Natural'Value (Msg.Value ("Required", "0")));
                  Eita  : Agpl.Chronos.Object; -- Expected Instant
                  Efta  : Agpl.Chronos.Object; -- Expected Filtered

                  Progress_Bar : constant String := "|" &
                    (Integer (Percent (Msg) / 5.0) * "*") &
                    Iif (Percent (Msg) = 100.0, "|", "");
               begin
                  Avg.Push (Float (Timer.Elapsed));
                  Efta.Reset (Duration (Total - Succ) * Duration (Avg.Average));
                  Eita.Reset (Duration (Total - Succ) * Timer.Elapsed);
                  Put_Line (Lpad (+Name, 14) &
                            ": [" &
                            Rpad (Percent_Str (Msg), 7) & "] [" &
                            Msg.Value ("Failed",    "?") & "/" &
                            Msg.Value ("Succeeded", "?") & "/" &
                            Msg.Value ("Required",  "?") & "/" &
                            Msg.Value ("Total",     "?") & "]" &
                            " [Total: " & Toter.Image & "]" &
                            " [Delta: " & Timer.Image & "]" &
--                            " [EITA: "   & Eita.Image & "]" &
                            " [ETA: "   & Efta.Image & "] " &
                            Progress_Bar
                           );
               end;
               Timer.Reset;
            end if;
         when Message.Finishedcompression =>
            Timer.Reset;
            Toter.Reset;
         when others =>
            if not Silent then
               Put_Line ((+Name) & ": " & Msg.Kind_Image);
            end if;
         end case;
   end Cb;

end Adafn.Callbacks;
