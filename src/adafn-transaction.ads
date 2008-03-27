with Adafn.Context;
with Adafn.Context.Handle;
with Adafn.Errors;
with Adafn.Identifier;
with Adafn.Inserts;
with Adafn.Key;
with Adafn.Message;
private with Adafn.Message.Handle;
with Adafn.Options;
with Adafn.Result;

private with Agpl.Chronos;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

--  Aglutinates messages around a single Id.

package Adafn.Transaction is

   type Kinds   is (Request, Insert, Nothing);
   type Actions is (Nothing, Remove);
   type States is  (Processing, Completed, Failed, Timeout);
   subtype Blocking_States is States range Completed .. Timeout;

   type Object (Kind : Kinds) is tagged private;
   --  Nothing is used elsewhere to denote an invalid or not found transaction

   Null_Object : constant Object (Nothing);

   type Callback is access
     procedure (This    : in     Object;
                Action  :    out Actions);
   --  Use these ones to get asynchronous informs of progress...

   function Create_Request
     (Id      : Identifier.Object;
      Uri     : Key.Object;
      Opts    : Options.For_Request;
      Cb      : Callback := null;
      Ctx     : Adafn.Context.Object'Class := Adafn.Context.Null_Object)
      return Object;

   function Create_Insert
     (Id      : Identifier.Object;
      Uri     : Inserts.Uri;
      Opts    : Options.For_Insert;
      Cb      : Callback := null;
      Ctx     : Adafn.Context.Object'Class := Adafn.Context.Null_Object)
      return Object;

   function "=" (L, R : Object) return Boolean;
   --  By id.

   function Is_Null  (This : Object) return Boolean;
   function Is_Valid (This : Object) return Boolean;

   procedure Add_Answer (This : in out Object; Answer : Message.Object'Class);

   procedure Add_Request (This : in out Object; Request : Message.Object'Class);

   function Age (This : Object) return Duration;

   function Cb (This : Object) return Callback;

   function Context (This : Object) return Adafn.Context.Object'Class;

   function Content_Type (This : Object) return String;

   procedure Set_Content_Type (This : in out Object; Ct : String);

   function Data (This : Object) return Result.Object;

   procedure Set_Data (This : in out Object; Data : Result.Object);

   procedure Set_Context (This : in out Object;
                          Ctx  :        Adafn.Context.Object'Class);

   function Id (This : Object) return Identifier.Object;

   function Last_Answer (This : Object) return Message.Object'Class;

   function Last_Request (This : Object) return Message.Object'Class;

   procedure Set_Status (This : in out Object; Status : States);

   function Status (This : Object) return States;

   function Timeout (This : Object) return Duration;

   function Request_Options (This : Object) return Options.For_Request;

   procedure Set_Request_Error (This : in out Object;
                                Err  :        Errors.For_Request);

   function Request_Error (This : Object) return Errors.For_Request;

   procedure Set_Request_Options (This : in out Object;
                                  Opts :        Options.For_Request);

   function Req_Uri (This : Object) return Key.Object;
   function Ins_Uri (This : Object) return Inserts.Uri;

   procedure Set_Req_Uri (This : in out Object; Uri : Key.Object);
   procedure Set_Ins_Uri (This : in out Object; Uri : Inserts.Uri);

private

   type Object (Kind : Kinds) is tagged record
      Id          : Identifier.Object;
      Status      : States := Processing;

      Nod_Cli_Msg : Message.Handle.Object;
      Cli_Nod_Msg : Message.Handle.Object;

      Cb          : Callback := null;
      Ctx         : Adafn.Context.Handle.Object;
      Started     : Agpl.Chronos.Object;

      Content_Type: Ustring;

      case Kind is
         when Insert =>
            Ins_Uri  : Inserts.Uri;
            Ins_Opts : Options.For_Insert;
            Ins_Err  : Errors.For_Insert;
         when Request =>
            Req_Uri  : Key.Object;
            Req_Opts : Options.For_Request;
            Req_Err  : Errors.For_Request;
            Data     : Result.Object;
         when Nothing =>
            null;
      end case;
   end record;

   Null_Object : constant Object (Nothing) := (Nothing, others => <>);

end Adafn.Transaction;
