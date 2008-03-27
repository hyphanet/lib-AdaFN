with Adafn.Metadata;
with Adafn.Key.Selftest;

with Agpl.Chronos;
with Agpl.Trace; use Agpl.Trace;
use Agpl;

with Ada.Io_Exceptions;

package body Adafn.Node is

   Poll_Delay : constant Duration := 0.5;

   -------------
   -- Connect --
   -------------

   procedure Connect (This : in out Object;
                      Name :        String                 := "AdaFN";
                      Host :        String                 := "127.0.0.1";
                      Port :        Gnat.Sockets.Port_Type := 9481)
   is
   begin
      This.Safe.Connect (Name, Host, Port);
   end Connect;

   ------------------
   -- Is_Connected --
   ------------------

   function Is_Connected (This : Object) return Boolean is
   begin
      return This.Safe.Is_Connected;
   end Is_Connected;

   -------------
   -- Get_Key --
   -------------

   procedure Get_Key (This    : in out Object;
                      Url     :        Key.Object;
                      Data    :    out Result.Object;
                      Result  :    out Transaction.Blocking_States;
                      Opts    :        Options.For_Request := Options.Default_For_Request)
   is
      Err : Errors.For_Request;
   begin
      This.Get_Key (Url, Data, Result, Err, Opts);
   end Get_Key;

   procedure Get_Key (This    : in out Object;
                      Url     :        Key.Object;
                      Data    :    out Result.Object;
                      Result  :    out Transaction.Blocking_States;
                      Err     :    out Errors.For_Request;
                      Opts    :        Options.For_Request := Options.Default_For_Request)
   is
      use Transaction;
      use type Errors.Error_Codes;
      Id       : Identifier.Object;
      Tran     : Transaction.Object (Request);
      New_Opts : Options.For_Request := Opts;
   begin
      --  Why these are forced?
      --  New_Opts.Global_Queue := False;
      --  New_Opts.Persistence  := Connection;
      New_Opts.Return_Type  := Direct;
      Get_Key (This, Url, Id, Opts => New_Opts);
      Wait_For_Transaction (This, Id, Tran);

      Result := Status (Tran);

      case Status (Tran) is
         when Completed =>
            Data := Transaction.Data (Tran);
         when Failed =>
            Err  := Transaction.Request_Error (Tran);
         when others =>
            null;
      end case;

      This.Safe.Remove_Transaction (Id);
   end Get_Key;

   -------------
   -- Get_Key --
   -------------

   procedure Get_Key (This    : in out Object;
                      Url     :        Key.Object;
                      Id      :    out Identifier.Object;
                      Opts    :        Options.For_Request := Options.Default_For_Request;
                      Cb      :        Transaction.Callback := null;
                      Ctx     : in     Context.Object'Class := Context.Null_Object)
   is
   begin
      This.Safe.Get_Key (Url, Id, Cb, Ctx, Opts);
   end Get_Key;

   ----------------
   -- Insert_Dir --
   ----------------

   procedure Insert_Dir (This : in out Object;
                         Url  :        Key.Object;
                         Path :        String;
                         Id   :    out Identifier.Object;
                         Opts :        Options.For_Insert;
                         Cb   :        Transaction.Callback := null;
                         Ctx  :        Context.Object'Class := Context.Null_Object)
   is
   begin
      This.Safe.Insert_Dir (Url, Path, Id, Opts, Cb, Ctx);
   end Insert_Dir;

   procedure Insert_Complex (This : in out Object;
                             Url  :        Key.Object;
                             Src  :        Source.Vectors.Vector;
                             Id   :    out Identifier.Object;
                             Opts :        Options.For_Insert   := Options.Default_For_Insert;
                             Cb   :        Transaction.Callback := null;
                             Ctx  :        Context.Object'Class := Context.Null_Object)
   is
   begin
      This.Safe.Insert_Complex (Url, Src, Id, Opts, Cb, Ctx);
   end Insert_Complex;

   -----------------
   -- Insert_File --
   -----------------

   procedure Insert_File (This : in out Object;
                          Src  :        Source.Object;
                          Uri  :        Inserts.Uri;
                          Id   :    out Identifier.Object;
                          Opts :        Options.For_Insert   := Options.Default_For_Insert;
                          Cb   :        Transaction.Callback := null;
                          Ctx  :        Context.Object'Class := Context.Null_Object)
   is
   begin
      This.Safe.Insert_File (Src, Uri, Id, Opts, Cb, Ctx);
   end Insert_File;

   ----------
   -- Name --
   ----------

   function Name (This : Object) return String is
   begin
      return This.Safe.Get_Name;
   end Name;

   ------------------------
   -- Cancel_Transaction --
   ------------------------

   procedure Cancel_Transaction (This : in out Object; Id : Identifier.Object) is
   begin
      This.Safe.Cancel_Transaction (Id);
   end Cancel_Transaction;

   ------------------------
   -- Remove_Transaction --
   ------------------------

   procedure Remove_Transaction (This : in out Object; Id : Identifier.Object)
   is
   begin
      This.Safe.Remove_Transaction (Id);
   end Remove_Transaction;

   ------------------
   -- Transactions --
   ------------------

   function Transactions (This : Object) return Natural is
   begin
      return This.Safe.Transaction_Count;
   end Transactions;

   --------------------------
   -- Wait_For_Transaction --
   --------------------------

   procedure Wait_For_Transaction (This : in out Object;
                                   Id   : in     Identifier.Object;
                                   Tran :    out Transaction.Object)
   is
      use Transaction;
   begin
      loop
         Tran := This.Safe.Get_Transaction (Id);
         exit when Status (Tran) /= Processing;
         delay Poll_Delay;
      end loop;
   end Wait_For_Transaction;

   ----------
   -- Data --
   ----------

   function Get_Data (This : Object; Id : Identifier.Object) return Result.Object is
   begin
      return Transaction.Data (This.Get_Transaction (Id));
   end Get_Data;

   ---------------------
   -- Get_Transaction --
   ---------------------

   function Get_Transaction (This : Object;
                             Id   : Identifier.Object) return Transaction.Object
   is
   begin
      return This.Safe.Get_Transaction (Id);
   end Get_Transaction;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (This : in out Object) is
   begin
      This.Active.Shutdown;
      This.Safe.Shutdown;
   end Shutdown;

   -------------------
   -- Active_Object --
   -------------------

   task body Active_Object is
      Done : Boolean := False;

      Timer_Tout : Chronos.Object;
   begin
      while not Done loop
         select
            accept Shutdown;
            Done := True;
         or
            delay Poll_Delay;
         end select;

         exit when Done;

         begin
            if This.Safe.Is_Connected then
               This.Safe.Poll;
            end if;
         exception
            when E : others =>
               Log ("AdaFN.Node.Active.Poll:" & Report (E), Error);
         end;

         if Timer_Tout.Elapsed >= 1.0 then
            Timer_Tout.Reset;
            This.Safe.Check_Timeouts;
         end if;
      end loop;
   exception
      when E : others =>
         Log ("AdaFN.Node.Active:" & Report (E), Error);
   end Active_Object;

   -----------------
   -- Safe_Object --
   -----------------

   protected body Safe_Object is

      ------------------------
      -- Cancel_Transaction --
      ------------------------

      procedure Cancel_Transaction (Id : Identifier.Object) is
         Msg : Message.Object (Message.Removepersistentrequest);
      begin
         Msg.Set_Value ("Global", "False");
         Msg.Set_Value ("Identifier", Identifier.Image (Id));
         Send (Msg);
         Remove_Transaction (Id);
      end Cancel_Transaction;

      -------------
      -- Connect --
      -------------

      procedure Connect (Name : String;
                         Host : String;
                         Port : Gnat.Sockets.Port_Type)
      is
         use Gnat.Sockets;
         Address : Sock_Addr_Type;
      begin
         Safe_Object.Name := + Name;

         Gnat.Sockets.Initialize;
         Create_Socket (Link, Family_Inet, Socket_Stream);
         Address.Addr := Inet_Addr (Host);
         Address.Port := Port;
         Connect_Socket (Link, Address);
         Stream       := Gnat.Sockets.Stream (Link);

         --  Prepare for polling
         Create_Selector (Poller);

         --  Send hello
         Send (Message.Client_Hello (Name));

         Connected := True;
      end Connect;

      --------------------
      -- Check_Timeouts --
      --------------------

      procedure Check_Timeouts is
         use Transaction_Maps;
         I : Cursor := Transactions.Last;
         J : Cursor;
      begin
         while Has_Element (I) loop
            J := Previous (I);
            declare
               use Transaction;
               Tran : Transaction.Object := Element (I);
            begin
               case Status (Tran) is
                  when Processing =>
                     if Age (Tran) > Timeout (Tran) then
                        Set_Status (Tran, Timeout);
                        Do_Callback (Tran);
                     end if;
                  when others =>
                     null;
               end case;
               end;
            I := J;
         end loop;
      end Check_Timeouts;

      -----------------
      -- Do_Callback --
      -----------------

      procedure Do_Callback (Tran : in out Transaction.Object) is
         use Transaction;
         Act : Actions := Nothing;
      begin
         if Cb (Tran) /= null then
            Cb (Tran) (Tran, Act);
         end if;
         if Act = Remove then
            Transactions.Delete (Id (Tran));
         else
            Transactions.Include (Id (Tran), Tran);
         end if;
      exception
         when E : others =>
            Log ("Do_Callback: " & Report (E), Warning);
      end Do_Callback;

      -------------
      -- Get_Key --
      -------------

      procedure Get_Key (Url     :        Key.Object;
                         Id      :    out Identifier.Object;
                         Cb      :        Transaction.Callback := null;
                         Ctx     : in     Context.Object'Class := Context.Null_Object;
                         Opts    :        Options.For_Request := Options.Default_For_Request)
      is
      begin
         --  Id := Identifier.Generate ((+Name) & "-");
         Id := Identifier.Value ((+Name) & "-" & Key.Image (Url));
         declare
            Tran : Transaction.Object :=
                     Transaction.Create_Request (Id, Url, Opts, Cb, Ctx);
            Req  : constant Message.Object'Class :=
                     Message.Client_Get (Id, Url, Opts);
         begin
            Transaction.Add_Request (Tran, Req);
            Transactions.Insert (Id, Tran);
            Send (Req);
         end;
      end Get_Key;

      ----------------
      -- Insert_Dir --
      ----------------

      procedure Insert_Dir (Url  :        Key.Object;
                            Path :        String;
                            Id   :    out Identifier.Object;
                            Opts :        Options.For_Insert;
                            Cb   :        Transaction.Callback := null;
                            Ctx  :        Context.Object'Class := Context.Null_Object)
      is
      begin
         Id := Identifier.Generate ((+Name) & "-");
         declare
            Tran : Transaction.Object :=
                     Transaction.Create_Insert
                       (Id,
                        Inserts.Create (Key.Kind (Url), Url),
                        Opts, Cb, Ctx);
            Req  : constant Message.Object'Class :=
                     Message.Client_Put_Complex_Dir (Id, Url, Path, Opts);
         begin
            Transaction.Add_Request (Tran, Req);
            Transactions.Insert (Id, Tran);
            Send (Req);
         end;
      end Insert_Dir;

      --------------------
      -- Insert_Complex --
      --------------------

      procedure Insert_Complex
        (Url  :     Key.Object;
         Src  :     Source.Vectors.Vector;
         Id   : out Identifier.Object;
         Opts :     Options.For_Insert   := Options.Default_For_Insert;
         Cb   :     Transaction.Callback := null;
         Ctx  :     Context.Object'Class := Context.Null_Object)
      is
      begin
         Id := Identifier.Generate ((+Name) & "-");
         declare
            Tran : Transaction.Object :=
                     Transaction.Create_Insert
                       (Id,
                        Inserts.Create (Key.Kind (Url), Url),
                        Opts, Cb, Ctx);
            Req  : constant Message.Object'Class :=
                     Message.Client_Put_Complex_Dir (Id, Url, Src, Opts);
         begin
            Transaction.Add_Request (Tran, Req);
            Transactions.Insert (Id, Tran);
            Send (Req);
         end;
      end Insert_Complex;

      -----------------
      -- Insert_File --
      -----------------

      procedure Insert_File
        (Src  :        Source.Object;
         Uri  :        Inserts.Uri;
         Id   :    out Identifier.Object;
         Opts :        Options.For_Insert   := Options.Default_For_Insert;
         Cb   :        Transaction.Callback := null;
         Ctx  :        Context.Object'Class := Context.Null_Object)
      is
      begin
         Id := Identifier.Generate ((+Name) & "-");
         declare
            Tran : Transaction.Object :=
                     Transaction.Create_Insert (Id, Uri, Opts, Cb, Ctx);
            Req  : constant Message.Object'Class :=
                     Message.Client_Put (Id, Uri, Src, Opts);
         begin
            Transaction.Add_Request (Tran, Req);
            Transactions.Insert (Id, Tran);
            Send (Req);
         end;
      end Insert_File;

      --------------
      -- Get_Name --
      --------------

      function Get_Name return String is
      begin
         return +Name;
      end Get_Name;

      ---------------------
      -- Get_Transaction --
      ---------------------

      function Get_Transaction (Id :   Identifier.Object)
                                return Transaction.Object
      is
         use Transaction_Maps;
         I     : constant Cursor := Transactions.Find (Id);
         Found : Boolean;
      begin
         Found := Has_Element (I);
         if Found then
            return Element (I);
         else
            return Transaction.Null_Object;
         end if;
      end Get_Transaction;

      ------------------
      -- Is_Connected --
      ------------------

      function Is_Connected return Boolean is
      begin
         return Connected;
      end Is_Connected;

      ----------
      -- Poll --
      ----------

      procedure Poll is
         use Gnat.Sockets;
         Read_Set  : Socket_Set_Type;
         Write_Set : Socket_Set_Type;
         Status    : Selector_Status;
      begin
         loop
            begin
               Empty (Read_Set);
               Empty (Write_Set);
               Set   (Read_Set, Link);

               --  Poll new messages:
               Check_Selector
                 (Poller,
                  Read_Set,
                  Write_Set,
                  Status,
                  Immediate);

               if Status = Completed then
                  declare
                     Msg : constant Message.Object'Class := Message.Read (Stream);
                  begin
                     Process_Msg (Msg);
                  end;
               else
                  exit;
               end if;
            exception
               when E : Gnat.Sockets.Socket_Error |
                        Ada.Io_Exceptions.End_Error |
                        Ada.Io_Exceptions.Data_Error =>
                  Connected := False;
                  Log ("Node.Poll: " & Report (E), Error);
                  exit;
               when E : others =>
                  Log ("Node.Poll: " & Report (E), Error);
                  exit;
            end;
         end loop;
      end Poll;

      ------------------------
      -- Process_Get_Failed --
      ------------------------

      procedure Process_Get_Failed (Msg  :        Message.Object'Class;
                                    Tran : in out Transaction.Object)
      is
         use Errors;
         use Transaction;
--           Code : constant Errors.Error_Codes :=
--                    Errors.Error_Codes'Value (Msg.Value ("Code"));
         Opts : Options.For_Request := Tran.Request_Options;

         May_Redirect : constant Boolean := Msg.Contains ("RedirectURI");
      begin
         Tran.Set_Request_Error
           (Errors.For_Request'(Errors.Create_From_Message (Msg)));

         if May_Redirect then
               if Opts.Follow_Redirects > 0 then
                  Opts.Follow_Redirects := Opts.Follow_Redirects - 1;
                  Tran.Set_Request_Options (Opts);
                  Tran.Set_Req_Uri (Key.Value (Msg.Value ("RedirectURI")));
                  declare
                     Req  : constant Message.Object'Class :=
                              Message.Client_Get (Tran.Id, Tran.Req_Uri, Opts);
                  begin
                     Send (Req);
                  end;
                  Log ("Following redirect for request " & Identifier.Image (Id (Tran)),
                       Debug, Log_Section);
               else
                  Log ("GetFailed [Non-following redirect] for request " & Identifier.Image (Id (Tran)),
                       Debug, Log_Section);
                  Tran.Set_Status (Failed);
            end if;
         else
            Log ("GetFailed [Non-redirectable] for request " &
                 Identifier.Image (Id (Tran)),
                 Debug, Log_Section);
            Tran.Set_Status (Failed);
         end if;

         Do_Callback (Tran);
      end Process_Get_Failed;

      -----------------
      -- Process_Msg --
      -----------------

      procedure Process_Msg (Msg : Message.Object'Class) is
         use Message;
         use Transaction;
         Tran : Transaction.Object :=
                  Get_Transaction
                    (Identifier.Value (Msg.Value_Or_Default ("Identifier", "")));
      begin
         if Tran.Is_Valid then
            Add_Answer (Tran, Msg);
            Update_Transaction (Tran);
         elsif Msg.Value_Or_Default ("Identifier", "") /= "" then
            Log
              ("Untracked transaction: " & Msg.Kind_Image &
               " [Id:" & Msg.Value_Or_Default ("Identifier", "<not supplied>") &
               "]",
               Warning);
         else
            Log
              ("Untracked transaction: " & Msg.Kind_Image &
               " [Id:" & Msg.Value_Or_Default ("Identifier", "<not supplied>") &
               "]",
               Debug);
         end if;

         case Msg.Kind is
            when Datafound =>
               if Tran.Is_Valid then
                  --  Extract metadata:
                  declare
                     Data : Result.Object := Tran.Data;
                  begin
                     Data.Meta := Metadata.Create (Msg.Value ("Metadata.ContentType"));
                     Tran.Set_Data (Data);
                  end;
                  Do_Callback (Tran);

                  --  Further processing?
                  if Request_Options (Tran).Return_Type = Direct and then
                    Request_Options (Tran).Persistence /= Connection then
                     --  Force the AllData message:
                     --  GetRequestData
                     raise Constraint_Error with "Unimplemented";
                  elsif Request_Options (Tran).Return_Type /= Direct then
                     --  Inform via callback, set status to Completed
                     raise Constraint_Error with "Unimplemented";
                  end if;
               end if;
            when Alldata =>
               if Tran.Is_Valid then
                  Log ("Data found for request " & Identifier.Image (Id (tran)), Debug, Log_Section);
                  Set_Status (tran, Completed);
                  declare
                     Data : Result.Object := Tran.Data;
                  begin
                     Data.Data      := Msg.Data;
                     Data.Meta.Size := Key_Size (ASU.Length (Data.Data));
                     Tran.Set_Data (Data);
                  end;
                  Do_Callback (tran);
               else
                  Log ("Received data for untracked request: " &
                       Identifier.Image (Id (tran)), Debug, Log_Section);
               end if;
            when Getfailed =>
               if Tran.is_Valid then
                  Process_Get_Failed (Msg, Tran);
               else
                  Log ("GetFailed for untracked request: " &
                       Msg.Value_Or_Default ("Identifier", ""),
                       Debug, Log_Section);
               end if;
            when Protocolerror =>
               if Tran.Is_Valid then
                  Set_Request_Error
                    (Tran,
                     Errors.For_Request'(Errors.Create_From_Message (Msg)));
                  Set_Status (Tran, Failed);
                  Do_Callback (Tran);
               end if;
               Msg.Debug_Dump;
            when Nodehello |
                 Finishedcompression |
                 Putfailed |
                 Putsuccessful |
                 Simpleprogress |
                 Startedcompression |
                 Urigenerated
                 =>
               if Tran.Is_Valid then
                  Do_Callback (Tran);
               end if;
            when others =>
               if Tran.Is_Valid then
                  Do_Callback (Tran);
               end if;
               Log ("Unprocessed message: " & Msg.Kind_Image, Warning);
         end case;
      end Process_Msg;

      ------------------------
      -- Remove_Transaction --
      ------------------------

      procedure Remove_Transaction (Id : Identifier.Object) is
      begin
         Transactions.Exclude (Id);
      end Remove_Transaction;

      ----------
      -- Send --
      ----------

      procedure Send (Msg : Message.Object'Class) is
      begin
         Msg.Write (Stream);
      end Send;

      --------------
      -- Shutdown --
      --------------

      procedure Shutdown is
      begin
         Gnat.Sockets.Close_Socket (Link);
      end Shutdown;

      -----------------------
      -- Transaction_Count --
      -----------------------

      function Transaction_Count return Natural is
      begin
         return Natural (Safe_Object.Transactions.Length);
      end Transaction_Count;

      ------------------------
      -- Update_Transaction --
      ------------------------

      procedure Update_Transaction (Tran : Transaction.Object) is
         use Transaction;
      begin
         Transactions.Include (Id (Tran), Tran);
      end Update_Transaction;

   end Safe_Object;

end Adafn.Node;
