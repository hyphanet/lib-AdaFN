with Adafn.Metadata; use adafn.metadata;

with Agpl.Filesystem;
with Agpl.Strings;
with Agpl.Strings.Fields;
with Agpl.Trace; use Agpl.Trace;
use Agpl;

with Ada.Characters.Latin_1;
with Ada.Directories;
--  with Ada.Text_Io; use Ada.Text_Io;

package body Adafn.Message is

   use Agpl.Containers.String_String_Maps;
   function K (I : Cursor) return String
               renames Agpl.Containers.String_String_Maps.Key;

   Lf : Character renames Ada.Characters.Latin_1.LF;

   Kind_Images :
   constant array (Kinds) of
     access String :=
       (Clienthello             => new String'("ClientHello"),
        Clientget               => new String'("ClientGet"),
        ClientPut               => new String'("ClientPut"),
        Clientputdiskdir        => new String'("ClientPutDiskDir"),
        Clientputcomplexdir     => new String'("ClientPutComplexDir"),
        Removepersistentrequest => new String'("RemovePersistentRequest"),
        Unimplemented           => new String'("Unimplemented"),
        Datafound               => new String'("DataFound"),
        Alldata                 => new String'("AllData"),
        Getfailed               => new String'("GetFailed"),
        Putfailed               => new String'("PutFailed"),
        PutSuccessful           => new String'("PutSuccessful"),
        Persistentput           => new String'("PersistentPut"),
        Persistentputdir        => new String'("PersistentPutDir"),
        Simpleprogress          => new String'("SimpleProgress"),
        Nodehello               => new String'("NodeHello"),
        StartedCompression      => new String'("StartedCompression"),
        FinishedCompression     => new String'("FinishedCompression"),
        URIGenerated            => new String'("URIGenerated"),
        Protocolerror           => new String'("ProtocolError"));

   function Kind_Image (This : Object) return String is
   begin
      return +This.Kind_Img;
   end Kind_Image;

   --------------
   -- Contains --
   --------------

   function Contains (This : Object; Key : String) return Boolean is
   begin
      return This.Values.Contains (Agpl.Strings.L (Key));
   end Contains;

   -----------
   -- Value --
   -----------

   function Value (This : Object; Key : String) return String is
      I : constant Cursor := This.Values.Find (Agpl.Strings.L (Key));
   begin
      if Has_Element (I) then
         return Element (I);
      else
         raise Value_Not_Present;
      end if;
   end Value;

   ----------------------
   -- Value_Or_Default --
   ----------------------

   function Value_Or_Default (This : Object;
                              Key  : String;
                              Def  : String) return String
   is
   begin
      return Value (This, Key);
   exception
      when Value_Not_Present =>
         return Def;
   end Value_Or_Default;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (This : in out Object; Key, Value : String) is
   begin
      This.Values.Include (Key, Value);
   end Set_Value;

   ---------------
   -- Read_Line --
   ---------------

   function Read_Line (Stream : access Ada.Streams.Root_Stream_Type'Class)
                       return          String is
      Line : String (1 .. 1000);
      Last : Natural := Line'First - 1;
   begin
      loop
         loop
            Line (Last + 1) := Character'Input (Stream);
            --  Put (Line (Last + 1)); Flush (Standard_Output);
            exit when Line (Last + 1) = Lf;
            Last := Last + 1;
         end loop;

         --  Skip empty lines
         exit when Last >= Line'First;
      end loop;

      Log ("Node -> Client: " & Line (Line'First .. Last), Debug, Log_Section);
      if Line (Line'First .. Last) = "EndMessage" then
         Log ("", Debug, Log_Section);
      end if;

      return Line (Line'First .. Last);
   end Read_Line;

   ----------
   -- Data --
   ----------

   function Data (This : Object) return Ustring is
   begin
      return This.Data;
   end Data;

   ----------
   -- Read --
   ----------

   function Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class)
      return Object'Class
   is
      use Agpl.Strings; use Agpl.Strings.Fields;
      Kind : Kinds;
      Kimg : constant String := Read_Line (Stream);
   begin
      begin
         Kind := Kinds'Value (Kimg);
      exception
         when Constraint_Error =>
            Kind := Unimplemented;
      end;

      declare
         Msg : Object (Kind);
      begin
         Msg.Kind_Img := +Kimg;
         loop
            declare
               Line  : constant String := Read_Line (Stream);
               Lline : constant String := L (Line);
            begin
               exit when Lline = "endmessage";

               if Lline = "data" then
                  Msg.Read_Data (Stream);
                  exit; -- No EndMessage expected
               else
                  Msg.Values.Insert
                    (Trim (Select_Field (Lline, 1, '=')),
                     Trim (Select_Field (Line, 2, '=')));
               end if;
            end;
         end loop;

         return Msg;
      end;
   end Read;

   ---------------
   -- Read_Data --
   ---------------

   procedure Read_Data (This : in out Object;
                        Stream : access Ada.Streams.Root_Stream_Type'Class)
   is
      Length : constant Natural := Natural'Value (This.Value ("DataLength"));
      Remain :          Natural := Length;
   begin
      pragma Assert (Asu.Length (This.Data) = 0);

      while Remain > 0 loop
         declare
            Chunk : String (1 .. Natural'Min (Remain, 1000));
         begin
            String'Read (Stream, Chunk);
            Asu.Append (This.Data, Chunk);
            Remain := Remain - Chunk'Length;
         end;
      end loop;
   end Read_Data;

   -----------
   -- Write --
   -----------

   procedure Write
     (This   : Object;
      Stream : access Ada.Streams.Root_Stream_Type'Class)
   is
      procedure Write_Value (I : Cursor) is
      begin
         Log ("Client -> Node: " & K (I) & "=" & Element (I), Debug, Log_Section);
         String'Write (Stream, K (I) & "=" & Element (I) & Lf);
      end Write_Value;
      use Asu;
   begin
      Log ("Client -> Node: " & Kind_Images (This.Kind).all, Debug, Log_Section);
      String'Write (Stream, Kind_Images (This.Kind).all & Lf);

      This.Values.Iterate (Write_Value'Access);

      Log ("Client -> Node: EndMessage", Debug, Log_Section);
      String'Write (Stream, "EndMessage" & Lf);

      if This.Data /= +"" then
         Log ("Client -> Node:" & Asu.Length (This.Data)'Img &
              " bytes of data being sent", Debug, Log_Section);
         --  Log ("Client -> Node:" & (+This.Data), Debug, Log_Section);
         String'Write (Stream, +This.Data);
         Log ("Client -> Node: Data sent", Debug, Log_Section);
      end if;

      Log ("", Debug, Log_Section);
   end Write;

   -----------
   -- Image --
   -----------

   function Image (This : Object) return String is
      Img : Ustring := +This.Kind'Img;

      procedure Add_Value (I : Cursor) is
      begin
         Asu.Append (Img, Lf & K (I) & " = " & Element (I));
      end Add_Value;
   begin
      This.Values.Iterate (Add_Value'Access);
      Asu.Append
        (Img, Lf & "Data:" & Natural'Image (Asu.Length (This.Data)) & " bytes");

      return +Img;
   end Image;

   ----------------
   -- Debug_Dump --
   ----------------

   procedure Debug_Dump (This : Object) is
   begin
      Log ("Message dump:" & Lf & This.Image, Always);
   end Debug_Dump;

   ------------------
   -- Client_Hello --
   ------------------

   function Client_Hello (Name : String) return Object'Class
   is
      Msg : Object (Clienthello);
   begin
      Msg.Values.Insert ("ExpectedVersion", "2.0");
      Msg.Values.Insert ("Name", Name);
      return Msg;
   end Client_Hello;

   ----------------
   -- Client_Get --
   ----------------

   function Client_Get (Id   : Identifier.Object;
                        Uri  : Key.Object;
                        Opts : Options.For_Request := Options.Default_For_Request)
                        return Object'Class
   is
      use Agpl.Strings;
      Msg : Object (Clientget);
   begin
      Msg.Set_Value ("Identifier",    Identifier.Image (Id));
      Msg.Set_Value ("URI",           Key.Image (Uri));
      Msg.Set_Value ("MaxSize",       Trim (Opts.Max_Size'Img));
      Msg.Set_Value ("PriorityClass", Trim (Opts.Priority'Img));
      Msg.Set_Value ("Persistence",   Opts.Persistence'Img);
      Msg.Set_Value ("Global",        Opts.Global_Queue'Img);
      Msg.Set_Value ("ReturnType",    Opts.Return_Type'Img);
      Msg.Set_Value ("MaxRetries",    Trim (Opts.Max_Retries'Img));
      Msg.Set_Value ("Verbose",       Trim (Opts.Verbose'Img));
      Msg.Set_Value ("DSonly",        Opts.From_Datastore'Img);

      return Msg;
   end Client_Get;

   ----------------
   -- Client_Put --
   ----------------

   function Client_Put
     (Id   : Identifier.Object;
      Uri  : Inserts.Uri;
      Src  : Source.Object;
      Opts : Options.For_Insert := Options.Default_For_Insert)
      return Object'Class
   is
      use Agpl.Strings;
      Msg : Object (Clientput);
   begin
      Msg.Set_Value ("URI",           Inserts.Image (Uri));
      Msg.Set_Value ("Identifier",    Identifier.Image (Id));
      Msg.Set_Value ("Verbosity",     Trim (Opts.Verbose'Img));
      Msg.Set_Value ("MaxRetries",    Trim (Opts.Max_Retries'Img));
      Msg.Set_Value ("PriorityClass", Trim (Opts.Priority'Img));
      Msg.Set_Value ("GetCHKOnly",    "False");
      Msg.Set_Value ("Persistence",   Capitalize (Opts.Persistence'Img));
      Msg.Set_Value ("Global",        Capitalize (Opts.Global_Queue'Img));
      Msg.Set_Value ("Metadata.ContentType",
                     Metadata.Guess_From_Filename (+Src.Name).Image);
      Msg.Set_Value ("DontCompress",  Capitalize (Boolean'Image (not Opts.Compress)));
--      Msg.Set_Value ("ClientToken",   Identifier.Image (Id));
      Msg.Set_Value ("UploadFrom",    Source.Upload_From (Src));
      case Src.Kind is
         when Source.Direct =>
            Msg.Set_Value ("DataLength", Trim (Asu.Length (Src.Data)'Img));
            Msg.Data := Src.Data;
         when Source.Directfromdisk =>
            Msg.Data := Agpl.Filesystem.Read_File (+Src.Path);
            Msg.Set_Value ("DataLength", Trim (Asu.Length (Msg.Data)'Img));
         when Source.Disk =>
            Msg.Set_Value ("Filename", +Src.Path);
         when Source.Redirect =>
            Msg.Set_Value ("TargetURI", Key.Image (Src.Uri));
      end case;
      return Msg;
   end Client_Put;

   -------------------------
   -- Client_Put_Disk_Dir --
   -------------------------

   function Client_Put_Disk_Dir
     (Id   : Identifier.Object;
      Uri  : Key.Object;
      Path : String;
      Opts : Options.For_Insert := Options.Default_For_Insert)
      return Object'Class
   is
      use Agpl.Strings;
      Msg : Object (Clientputdiskdir);
   begin
      if not Ada.Directories.Exists (Path) then
         raise Constraint_Error with "Path not found: " & Path;
      end if;

      Msg.Set_Value ("Identifier",    Identifier.Image (Id));
--      Msg.Set_Value ("ClientToken",   Identifier.Image (Id));
      Msg.Set_Value ("Verbosity",     Trim (Opts.Verbose'Img));
      Msg.Set_Value ("MaxRetries",    Trim (Opts.Max_Retries'Img));
      Msg.Set_Value ("PriorityClass", Trim (Opts.Priority'Img));
      Msg.Set_Value ("URI",           Key.Image (Uri));
      Msg.Set_Value ("GetCHKOnly",    "false");
      Msg.Set_Value ("DontCompress",  Capitalize (Boolean'Image (not Opts.Compress)));
      Msg.Set_Value ("Persistence",   Capitalize (Opts.Persistence'Img));
      Msg.Set_Value ("Global",        Capitalize (Opts.Global_Queue'Img));
      Msg.Set_Value ("DefaultName",   +Opts.Default_File); -- This seemed to fail
      Msg.Set_Value ("Filename",      Path);
      Msg.Set_Value ("AllowUnreadableFiles", Capitalize (Opts.Allow_If_Unreadable'Img));

      return Msg;
   end Client_Put_Disk_Dir;

   ---------------------------------
   -- Client_Put_Complex_Dir_Disk --
   ---------------------------------

   function Client_Put_Complex_Dir_Disk
     (Id   : Identifier.Object;
      Uri  : Key.Object;
      Path : String;
      Opts : Options.For_Insert := Options.Default_For_Insert)
      return Object'Class
   is
      use Agpl.Strings;
      Msg : Object (Clientputcomplexdir);
   begin
      if not Ada.Directories.Exists (Path) then
         raise Constraint_Error with "Path not found: " & Path;
      end if;

      Msg.Set_Value ("Identifier",    Identifier.Image (Id));
--      Msg.Set_Value ("ClientToken",   Identifier.Image (Id));
      Msg.Set_Value ("Verbosity",     Trim (Opts.Verbose'Img));
      Msg.Set_Value ("MaxRetries",    Trim (Opts.Max_Retries'Img));
      Msg.Set_Value ("PriorityClass", Trim (Opts.Priority'Img));
      Msg.Set_Value ("URI",           Key.Image (Uri));
      Msg.Set_Value ("GetCHKOnly",    "false");
      Msg.Set_Value ("DontCompress",  Capitalize (Boolean'Image (not Opts.Compress)));
      Msg.Set_Value ("Persistence",   Capitalize (Opts.Persistence'Img));
      Msg.Set_Value ("Global",        Capitalize (Opts.Global_Queue'Img));
      Msg.Set_Value ("DefaultName",   +Opts.Default_File);

      declare
         Count : Natural := 0;

         function Counter return String is
         begin
            return "Files." & Trim (Count'Img) & ".";
         end Counter;

         procedure Add_Folder (Path : String; Prefix : String) is
            use Ada.Directories;
            Files   : Search_Type;
            Folders : Search_Type;
         begin
            Start_Search (Files, Path, "",
                          (Ordinary_File => True, others => False));
            while More_Entries (Files) loop
               declare
                  File : Directory_Entry_Type;
               begin
                  Get_Next_Entry (Files, File);
                  Msg.Set_Value (Counter & "Name", Prefix & Simple_Name (File));
                  Msg.Set_Value (Counter & "UploadFrom", "disk");
                  Msg.Set_Value (Counter & "DataLength", Trim (Size (File)'Img));
                  Msg.Set_Value (Counter & "Filename", Full_Name (File));
                  Count := Count + 1;
               end;
            end loop;
            End_Search (Files);

            Start_Search (Folders, Path, "",
                          (Directory => True, others => False));
            while More_Entries (Folders) loop
               declare
                  Folder : Directory_Entry_Type;
               begin
                  Get_Next_Entry (Folders, Folder);
                  if Simple_Name (Folder) /= "." and then
                    Simple_Name (Folder) /= ".."
                  then
                     Add_Folder (Full_Name (Folder),
                                 Simple_Name (Folder) & "/");
                  end if;
               end;
            end loop;
            End_Search (Folders);
         end Add_Folder;
      begin
         Add_Folder (Path, "");
      end;

      return Msg;
   end Client_Put_Complex_Dir_Disk;

   -----------------------------------
   -- Client_Put_Complex_Dir_Direct --
   -----------------------------------

   function Client_Put_Complex_Dir_Direct
     (Id   : Identifier.Object;
      Uri  : Key.Object;
      Path : String;
      Opts : Options.For_Insert := Options.Default_For_Insert)
      return Object'Class
   is
      use Agpl.Strings;
      Sources : Source.Vectors.Vector;
   begin
      if not Ada.Directories.Exists (Path) then
         raise Constraint_Error with "Path not found: " & Path;
      end if;

      declare
         procedure Add_Folder (Path : String; Prefix : String) is
            use Ada.Directories;
            Files   : Search_Type;
            Folders : Search_Type;
         begin
            Start_Search (Files, Path, "",
                          (Ordinary_File => True, others => False));
            while More_Entries (Files) loop
               declare
                  File : Directory_Entry_Type;
                  use Asu;
               begin
                  Get_Next_Entry (Files, File);
                  Sources.Append
                    ((Kind => Source.Direct,
                      Name => + (Prefix & Simple_Name (File)),
                      Data => Agpl.Filesystem.Read_File (Full_Name (File))));
               end;
            end loop;
            End_Search (Files);

            Start_Search (Folders, Path, "",
                          (Directory => True, others => False));
            while More_Entries (Folders) loop
               declare
                  Folder : Directory_Entry_Type;
               begin
                  Get_Next_Entry (Folders, Folder);
                  if Simple_Name (Folder) /= "." and then
                    Simple_Name (Folder) /= ".."
                  then
                     Add_Folder (Full_Name (Folder),
                                 Simple_Name (Folder) & "/");
                  end if;
               end;
            end loop;
            End_Search (Folders);
         end Add_Folder;
      begin
         Add_Folder (Path, "");
      end;

      return Client_Put_Complex_Dir (Id, Uri, Sources, Opts);
   end Client_Put_Complex_Dir_Direct;

   ----------------------------
   -- Client_Put_Complex_Dir --
   ----------------------------

   function Client_Put_Complex_Dir
     (Id   : Identifier.Object;
      Uri  : Key.Object;
      Path : String;
      Opts : Options.For_Insert := Options.Default_For_Insert)
      return Object'Class
   is
   begin
      case Opts.Preferred_Send is
         when Direct =>
            return Client_Put_Complex_Dir_Direct (Id, Uri, Path, Opts);
         when Disk =>
            return Client_Put_Complex_Dir_Disk   (Id, Uri, Path, Opts);
         when others =>
            raise Constraint_Error
              with "Invalid send type: " & Opts.Preferred_Send'Img;
      end case;
   end Client_Put_Complex_Dir;

   ----------------------------
   -- Client_Put_Complex_Dir --
   ----------------------------

   function Client_Put_Complex_Dir
     (Id   : Identifier.Object;
      Uri  : Key.Object;
      Src  : Source.Vectors.Vector;
      Opts : Options.For_Insert := Options.Default_For_Insert)
      return Object'Class
   is
      use Agpl.Strings;
      Msg : Object (Clientputcomplexdir);

      ---------------
      -- Add_Entry --
      ---------------

      procedure Add_Entry (I : Natural; Src : Source.Object) is
         function Prefix return String is
         begin
            return "Files." & Trim (I'Img) & ".";
         end Prefix;
         Data : Ustring;
      begin
         Msg.Set_Value (Prefix & "Name", +Src.Name);
         Msg.Set_Value (Prefix & "UploadFrom", Source.Upload_From (Src));
         case Src.Kind is
            when Source.Redirect =>
               Msg.Set_Value (Prefix & "TargetURI", Key.Image (Src.Uri));
            when Source.Direct =>
               Msg.Set_Value (Prefix & "DataLength",
                              Trim (Asu.Length (Src.Data)'Img));
               Asu.Append (Msg.Data, Src.Data);
            when Source.Directfromdisk =>
               Data := Agpl.Filesystem.Read_File (+Src.Path);
               Asu.Append (Msg.Data, Data);
               Msg.Set_Value (Prefix & "DataLength", Trim (Asu.Length (Data)'Img));
            when Source.Disk =>
               Msg.Set_Value (Prefix & "Filename", +Src.Path);
         end case;
      end Add_Entry;
   begin
      Msg.Set_Value ("Identifier",    Identifier.Image (Id));
--      Msg.Set_Value ("ClientToken",   Identifier.Image (Id));
      Msg.Set_Value ("Verbosity",     Trim (Opts.Verbose'Img));
      Msg.Set_Value ("MaxRetries",    Trim (Opts.Max_Retries'Img));
      Msg.Set_Value ("PriorityClass", Trim (Opts.Priority'Img));
      Msg.Set_Value ("URI",           Key.Image (Uri));
      Msg.Set_Value ("GetCHKOnly",    "false");
      Msg.Set_Value ("DontCompress",  Capitalize (Boolean'Image (not Opts.Compress)));
      Msg.Set_Value ("Persistence",   Capitalize (Opts.Persistence'Img));
      Msg.Set_Value ("Global",        Capitalize (Opts.Global_Queue'Img));
      Msg.Set_Value ("DefaultName",   +Opts.Default_File);

      for I in Src.First_Index .. Src.Last_Index loop
         Add_Entry (I - 1, Src.Element (I));
      end loop;

      return Msg;
   end Client_Put_Complex_Dir;

end Adafn.Message;
