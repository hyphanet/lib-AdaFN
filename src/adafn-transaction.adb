with Adafn.Metadata;

package body Adafn.Transaction is

   --------------------
   -- Create_Request --
   --------------------

   function Create_Request
     (Id      : Identifier.Object;
      Uri     : Key.Object;
      Opts    : Options.For_Request;
      Cb      : Callback := null;
      Ctx     : Adafn.Context.Object'Class := Adafn.Context.Null_Object)
      return Object
   is
   begin
      return (Kind            => Request,
              Id              => Id,
              Req_Uri         => Uri,
              Req_Opts        => Opts,
              Cb              => Cb,
              Ctx             => Adafn.Context.Handle.Set (Ctx),
              Data            => (Uri => Uri, others => <>),
              others          => <>);
   end Create_Request;

   -------------------
   -- Create_Insert --
   -------------------

   function Create_Insert
     (Id      : Identifier.Object;
      Uri     : Inserts.Uri;
      Opts    : Options.For_Insert;
      Cb      : Callback := null;
      Ctx     : Adafn.Context.Object'Class := Adafn.Context.Null_Object)
      return Object
   is
   begin
      return (Kind            => Insert,
              Id              => Id,
              Ins_Uri         => Uri,
              Ins_Opts        => Opts,
              Cb              => Cb,
              Ctx             => Adafn.Context.Handle.Set (Ctx),
              others          => <>);
   end Create_Insert;

   ---------
   -- "=" --
   ---------

   function "=" (L, R : Object) return Boolean is
      use Identifier;
   begin
      return L.Kind = R.Kind and then
        (L.Kind = Nothing or else L.Id = R.Id);
   end "=";

   -------------
   -- Is_Null --
   -------------

   function Is_Null (This : Object) return Boolean is
   begin
      return This.Kind = Nothing;
   end Is_Null;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (This : Object) return Boolean is
   begin
      return This.Kind /= Nothing;
   end Is_Valid;

   ----------------
   -- Add_Answer --
   ----------------

   procedure Add_Answer (This : in out Object; Answer : Message.Object'Class) is
   begin
      This.Nod_Cli_Msg.Set (Answer);
   end Add_Answer;

   -----------------
   -- Add_Request --
   -----------------

   procedure Add_Request (This : in out Object; Request : Message.Object'Class) is
   begin
      This.Cli_Nod_Msg.Set (Request);
   end Add_Request;

   ---------
   -- Age --
   ---------

   function Age (This : Object) return Duration is
   begin
      return This.Started.Elapsed;
   end Age;

   --------
   -- Cb --
   --------

   function Cb (This : Object) return Callback is
   begin
      return This.Cb;
   end Cb;

   -------------
   -- Context --
   -------------

   function Context (This : Object) return Adafn.Context.Object'Class is
   begin
      return This.Ctx.Get;
   end Context;

   -----------------
   -- Set_Context --
   -----------------

   procedure Set_Context
     (This : in out Object;
      Ctx  :        Adafn.Context.Object'Class)
   is
   begin
      This.Ctx.Set (Ctx);
   end Set_Context;

   --------
   -- Id --
   --------

   function Id (This : Object) return Identifier.Object is
   begin
      return This.Id;
   end Id;

   -----------------
   -- Last_Answer --
   -----------------

   function Last_Answer (This : Object) return Message.Object'Class is
   begin
      return This.Nod_Cli_Msg.Get;
   end Last_Answer;

   ------------------
   -- Last_Request --
   ------------------

   function Last_Request (This : Object) return Message.Object'Class is
   begin
      return This.Cli_Nod_Msg.Get;
   end Last_Request;

   ----------------
   -- Set_Status --
   ----------------

   procedure Set_Status (This : in out Object; Status : States) is
   begin
      This.Status := Status;
   end Set_Status;

   ------------
   -- Status --
   ------------

   function Status (This : Object) return States is
   begin
      return This.Status;
   end Status;

   -------------
   -- Timeout --
   -------------

   function Timeout (This : Object) return Duration is
   begin
      case This.Kind is
         when Request =>
            return This.Request_Options.Timeout;
         when Insert =>
            return This.Ins_Opts.Timeout;
         when Nothing =>
            raise Constraint_Error with "Transaction is Null";
      end case;
   end Timeout;

   ---------------------
   -- Request_Options --
   ---------------------

   function Request_Options (This : Object) return Options.For_Request is
   begin
      return This.Req_Opts;
   end Request_Options;

   ----------
   -- Data --
   ----------

   function Data (This : Object) return Result.Object is
   begin
      return This.Data;
   end Data;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data (This : in out Object; Data : Result.Object) is
   begin
      This.Data := Data;
   end Set_Data;

   ------------------
   -- Content_Type --
   ------------------

   function Content_Type (This : Object) return String is
   begin
      return +This.Content_Type;
   end Content_Type;

   ----------------------
   -- Set_Content_Type --
   ----------------------

   procedure Set_Content_Type (This : in out Object; Ct : String) is
   begin
      This.Content_Type   := +Ct;
      This.Data.Meta.Mime := Metadata.Value (Ct);
   end Set_Content_Type;

   -----------------------
   -- Set_Request_Error --
   -----------------------

   procedure Set_Request_Error (This : in out Object;
                                Err  :        Errors.For_Request)
   is
   begin
      This.Req_Err := Err;
   end Set_Request_Error;

   -------------------
   -- Request_Error --
   -------------------

   function Request_Error (This : Object) return Errors.For_Request is
   begin
      return This.Req_Err;
   end Request_Error;

   -------------------------
   -- Set_Request_Options --
   -------------------------

   procedure Set_Request_Options (This : in out Object;
                                  Opts :        Options.For_Request)
   is
   begin
      This.Req_Opts := Opts;
   end Set_Request_Options;

   -------------
   -- Req_Uri --
   -------------

   function Req_Uri (This : Object) return Key.Object is
   begin
      return This.Req_Uri;
   end Req_Uri;

   -------------
   -- Ins_Uri --
   -------------

   function Ins_Uri (This : Object) return Inserts.Uri is
   begin
      return This.Ins_Uri;
   end Ins_Uri;

   -----------------
   -- Set_Req_Uri --
   -----------------

   procedure Set_Req_Uri (This : in out Object; Uri : Key.Object) is
   begin
      This.Req_Uri  := Uri;
      This.Data.Uri := Uri;
   end Set_Req_Uri;

   -----------------
   -- Set_Ins_Uri --
   -----------------

   procedure Set_Ins_Uri (This : in out Object; Uri : Inserts.Uri) is
   begin
      This.Ins_Uri := Uri;
   end Set_Ins_Uri;

end Adafn.Transaction;
