with Adafn.Context;
with Adafn.Errors;
with Adafn.Key;
with Adafn.Identifier;
with Adafn.Inserts;
with Adafn.Message;
with Adafn.Options;
with Adafn.Result;
with Adafn.Source;
with Adafn.Source.Vectors;
with Adafn.Transaction;
with Adafn.Transaction.Containers;

with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Gnat.Sockets;

package Adafn.Node is

   --  A connection to the freenet node.
   --  Thread-safe.
   --  Several threads can block on blocking operations, and the first one
   --  to complete will return (i.e. not in FIFO order).
   --  Only one thread is used for callbacks, so a single threaded program
   --  should not worry about concurrency.
   --  Calling the node withing a callback *WILL DEADLOCK*

   type Object is tagged limited private;

   type Object_Access is access all Object'Class;

   procedure Connect (This : in out Object;
                      Name :        String                 := "AdaFN";
                      Host :        String                 := "127.0.0.1";
                      Port :        Gnat.Sockets.Port_Type := 9481);
   --  Stablishes connection and performs handshake.
   --  Upon call return, the connection is usable (if no exceptions occurred)
   --  The name should be unique!

   function Is_Connected (This : Object) return Boolean;

   function Name (This : Object) return String;

   procedure Get_Key (This    : in out Object;
                      Url     :        Key.Object;
                      Data    :    out Result.Object;
                      Result  :    out Transaction.Blocking_States;
                      Opts    :        Options.For_Request := Options.Default_For_Request);
   --  Blocking retrieval of data.
   --  Will override: persistence := Direct

   procedure Get_Key (This    : in out Object;
                      Url     :        Key.Object;
                      Data    :    out Result.Object;
                      Result  :    out Transaction.Blocking_States;
                      Err     :    out Errors.For_Request;
                      Opts    :        Options.For_Request := Options.Default_For_Request);
   --  As previous but also give error feedback

   procedure Get_Key (This    : in out Object;
                      Url     :        Key.Object;
                      Id      :    out Identifier.Object;
                      Opts    :        Options.For_Request := Options.Default_For_Request;
                      Cb      :        Transaction.Callback := null;
                      Ctx     : in     Context.Object'Class := Context.Null_Object);
   --  Non-blocking request, gives the identifier of the transaction and set
   --  an optional callback and context

   function Get_Data (This : Object; Id : Identifier.Object) return Result.Object;

   function Get_Transaction (This : Object;
                             Id   : Identifier.Object) return Transaction.Object;

   procedure Insert_Dir (This : in out Object;
                         Url  :        Key.Object;
                         Path :        String;
                         Id   :    out Identifier.Object;
                         Opts :        Options.For_Insert;
                         Cb   :        Transaction.Callback := null;
                         Ctx  :        Context.Object'Class := Context.Null_Object);
   --  Non-blocking inserting of all files in a folder

   procedure Insert_Complex (This : in out Object;
                             Url  :        Key.Object;
                             Src  :        Source.Vectors.Vector;
                             Id   :    out Identifier.Object;
                             Opts :        Options.For_Insert   := Options.Default_For_Insert;
                             Cb   :        Transaction.Callback := null;
                             Ctx  :        Context.Object'Class := Context.Null_Object);
   --  Url must be a SSK or USK; This will be translated into a ClientPutComplexDir

   procedure Insert_File (This : in out Object;
                          Src  :        Source.Object;
                          Uri  :        Inserts.Uri;
                          Id   :    out Identifier.Object;
                          Opts :        Options.For_Insert   := Options.Default_For_Insert;
                          Cb   :        Transaction.Callback := null;
                          Ctx  :        Context.Object'Class := Context.Null_Object);
   --  Non blocking insertion of a file

   function Transactions (This : Object) return Natural;
   --  Says the amount of transactions being tracked.

   procedure Cancel_Transaction (This : in out Object; Id : Identifier.Object);

   procedure Remove_Transaction (This : in out Object; Id : Identifier.Object);

   procedure Shutdown (This : in out Object);

private

   package Transaction_Maps renames Transaction.Containers.Maps;

   task type Active_Object (This : access Object) is
      entry Shutdown;
   end Active_Object;

   protected type Safe_Object (This : access Object) is
      procedure Connect (Name : String;
                         Host : String;
                         Port : Gnat.Sockets.Port_Type);
      function Is_Connected return Boolean;
      procedure Check_Timeouts;
      procedure Do_Callback (Tran : in out Transaction.Object);
      procedure Get_Key
        (Url     :     Key.Object;
         Id      : out Identifier.Object;
         Cb      :     Transaction.Callback := null;
         Ctx     :     Context.Object'Class := Context.Null_Object;
         Opts    :     Options.For_Request := Options.Default_For_Request);
      procedure Insert_Dir
        (Url  :     Key.Object;
         Path :     String;
         Id   : out Identifier.Object;
         Opts :     Options.For_Insert;
         Cb   :     Transaction.Callback := null;
         Ctx  :     Context.Object'Class := Context.Null_Object);
      procedure Insert_Complex
        (Url  :     Key.Object;
         Src  :     Source.Vectors.Vector;
         Id   : out Identifier.Object;
         Opts :     Options.For_Insert   := Options.Default_For_Insert;
         Cb   :     Transaction.Callback := null;
         Ctx  :     Context.Object'Class := Context.Null_Object);
      procedure Insert_File
        (Src  :     Source.Object;
         Uri  :     Inserts.Uri;
         Id   : out Identifier.Object;
         Opts :     Options.For_Insert   := Options.Default_For_Insert;
         Cb   :     Transaction.Callback := null;
         Ctx  :     Context.Object'Class := Context.Null_Object);
      function Get_Name return String;
      function Get_Transaction (Id : Identifier.Object) return Transaction.Object;
      procedure Poll;
      procedure Process_Msg (Msg : Message.Object'Class);
      function Transaction_Count return Natural;
      procedure Send (Msg : Message.Object'Class);
      procedure Shutdown;
      procedure Cancel_Transaction (Id : Identifier.Object);
      procedure Remove_Transaction (Id : Identifier.Object);
      procedure Update_Transaction (Tran : Transaction.Object);
   private
      Name   : Ustring;
      Link   : Gnat.Sockets.Socket_Type;
      Stream : Gnat.Sockets.Stream_Access;
      Poller : Gnat.Sockets.Selector_Type;

      Transactions : Transaction_Maps.Map;

      Connected : Boolean;
   end Safe_Object;

   type Object is tagged limited record
      Active : Active_Object (Object'Access);
      Safe   : Safe_Object (Object'Access);
   end record;

   procedure Wait_For_Transaction (This : in out Object;
                                   Id   : in     Identifier.Object;
                                   Tran :    out Transaction.Object);
   --  Until completion, failure or timeout...

end Adafn.Node;
