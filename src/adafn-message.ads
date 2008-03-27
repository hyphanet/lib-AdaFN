with Adafn.Identifier;
with Adafn.Inserts;
with Adafn.Key;
with Adafn.Options;
with Adafn.Source;
with Adafn.Source.Vectors;

with Agpl.Containers.String_String_Maps;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Ada.Streams;

--  A single communication with the node.

package Adafn.Message is

   --  pragma Preelaborate;

   Value_Not_Present : exception;

   type Kinds is (
                  --  Client to node
                  Clienthello,
                  Clientget,
                  Clientput,
                  Clientputdiskdir,
                  Clientputcomplexdir,
                  Removepersistentrequest,
                  --  Node to client
                  Datafound,
                  Alldata,
                  Getfailed,
                  Persistentput,
                  Persistentputdir,
                  Protocolerror,
                  Putfailed,
                  Putsuccessful,
                  Simpleprogress,
                  Startedcompression,
                  Finishedcompression,
                  Urigenerated,
                  Nodehello,
                  --  Unknown
                  Unimplemented
                  );

   type Object (Kind : Kinds) is tagged private;

   function Kind_Image (This : Object) return String;

   function Contains (This : Object; Key : String) return Boolean;

   function Value (This : Object; Key : String) return String;

   function Value_Or_Default (This : Object;
                              Key  : String;
                              Def  : String) return String;
   function Value            (This : Object;
                              Key  : String;
                              Def  : String) return String
                              renames Value_Or_Default;

   function Data (This : Object) return Ustring;
   --  Get the full data as an ustring (it could be too large to fit in stack).

   procedure Set_Value (This : in out Object; Key, Value : String);
   --  Add a value pair to an object

   function Read (Stream : access Ada.Streams.Root_Stream_Type'Class)
                  return          Object'Class;
   --  Parse from some stream

   procedure Write (This   : Object;
                    Stream : access Ada.Streams.Root_Stream_Type'Class);
   --  Dump it to some stream

   function Image (This : Object) return String;
   --  An utf8 string (with newlines within!)

   procedure Debug_Dump (This : Object);
   --  Write to logs.

   --  MESSAGE CREATION FUNCTIONS --
   function Client_Hello (Name : String) return Object'Class;

   function Client_Get (Id   : Identifier.Object;
                        Uri  : Key.Object;
                        Opts : Options.For_Request := Options.Default_For_Request)
                        return Object'Class;

   function Client_Put
     (Id   : Identifier.Object;
      Uri  : Inserts.Uri;
      Src  : Source.Object;
      Opts : Options.For_Insert := Options.Default_For_Insert)
      return Object'Class;

   function Client_Put_Disk_Dir
     (Id   : Identifier.Object;
      Uri  : Key.Object;
      Path : String;
      Opts : Options.For_Insert := Options.Default_For_Insert)
      return Object'Class;
   --  Insert a folder via ClientPutDiskDir. Will restart in node restart

   function Client_Put_Complex_Dir_Direct
     (Id   : Identifier.Object;
      Uri  : Key.Object;
      Path : String;
      Opts : Options.For_Insert := Options.Default_For_Insert)
      return Object'Class;
   --  use ClientPutComplexDir, recursive in folders, reading files locally
   --  and sending them to the node

   function Client_Put_Complex_Dir_Disk
     (Id   : Identifier.Object;
      Uri  : Key.Object;
      Path : String;
      Opts : Options.For_Insert := Options.Default_For_Insert)
      return Object'Class;
   --  As previous but filenames are sent to the node so it reads them
   --  Will only work with local nodes thus

   function Client_Put_Complex_Dir
     (Id   : Identifier.Object;
      Uri  : Key.Object;
      Src  : Source.Vectors.Vector;
      Opts : Options.For_Insert := Options.Default_For_Insert)
      return Object'Class;

   function Client_Put_Complex_Dir
     (Id   : Identifier.Object;
      Uri  : Key.Object;
      Path : String;
      Opts : Options.For_Insert := Options.Default_For_Insert)
      return Object'Class;
   --  Calls _Direct or _Disk depending on Opts.Preferred_Send

private

   type Message_States is (Processing, Completed);

   type Object (Kind : Kinds) is tagged record
      Kind_Img : Ustring;
      Values   : Agpl.Containers.String_String_Maps.Map;
      Data     : Ustring;
   end record;

   procedure Read_Data (This   : in out Object;
                        Stream : access Ada.Streams.Root_Stream_Type'Class);
   --  Assumes the datalength field is already present!

end Adafn.Message;
