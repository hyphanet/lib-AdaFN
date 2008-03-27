 

with Agpl.Streams.Memory_Arrays;
with Agpl.Conversions; use Agpl.Conversions;
with Agpl.Endian;

with Ada.Unchecked_conversion;

with Interfaces;                 use Interfaces;

package body Agpl.G2.Packet.Parsing is

   Null_Packet : Packet.Object;

   use type Agpl.Streams.Stream_Element_array_access;

   function To_control_byte is new
      Unchecked_conversion(Unsigned_8, Control_byte_type);
   function From_control_byte is new
      Unchecked_conversion(Control_byte_type, Unsigned_8);

   --  Returns a newly allocated G2 packet from a stream.
   --  Pre: The stream holds enough ready data for the packet or
   --    it will try to block.
   --  Pre: The control byte and len have been read from the stream and
   --    the stream position is in the name field.
   function From_stream (
      Control_byte : in Control_byte_type;
      Length       : in Natural;
      Stream       : access Ada.Streams.Root_stream_type'Class)
      return Packet.Object is

      P : Packet.Object;
      C : Child_access;
      Dummy : Natural;

   begin
      From_stream (Control_byte, Length, Stream, C, Dummy);
      if C = null then
         return Null_packet;
      else
         Bind (P, C);
         return P;
      end if;
   end From_Stream;

   --  Entirely from stream:
   function From_stream (
      Stream       : access Ada.Streams.Root_stream_type'Class)
      return Packet.Object is
      P : Packet.Object;
      C : Child_access;
      Dummy : Natural;
   begin
      From_stream (Stream, C, Dummy);
      Bind (P, C);
      return P;
   end From_stream;

   --  Returns a newly allocated G2 packet from a stream element array.
   function From_element_array (Data : in Ada.Streams.Stream_element_array)
      return Packet.Object is
      M : aliased Agpl.Streams.Memory_Arrays.Stream_type (Data'Unrestricted_Access);
      P : Packet.Object;
      C : Child_access;
      Dummy : Natural;
   begin
      From_stream (M'Access, C, Dummy);
      Bind (P, C);
      return P;
   end From_element_array;

   --  Returns a newly allocated G2 packet from a stream.
   --  Pre: The stream holds enough ready data for the packet or
   --    it will try to block.
   --  Pre: The control byte and len have been read from the stream and
   --    the stream position is in the name field.
   procedure From_stream (
      Control_byte : in Control_byte_type;
      Length       : in Natural;
      Stream       : access Ada.Streams.Root_stream_type'Class;
      Child        : out Packet.Child_access;
      Read         : out Natural)
   is

      P            : Packet.Child_access:= new Packet.Child;
      P_child      : Packet.Child_access;
      Children_size: Natural := 0;
      Child_size   : Natural;

   begin

      Read := 0;

--      Trace.Log (" ------> ");

      --  Read the control byte:
      P.Control_byte := Control_byte;

      --  Read length:
      P.Len := Length;

--      Trace.Log ("Len_len:" & P.Control_byte.Len_len'Img);
--      Trace.Log ("Name_len:" & P.Control_byte.Name_len'Img);
--      Trace.Log ("Len:" & P.Len'Img);

      --  Read name:
      declare
         Name : String (1 .. P.Control_byte.Name_len + 1);
      begin
         String'Read (Stream, Name);
         P.Type_name := B (Name);
         Read        := Read + Name'Length;
--         Trace.Log ("Name: " & Name);
      end;

      --  TRICK ONLY FOR BUG IN SHAREAZA 1.8.11.2
      if True and then S (P.Type_name) = "CH" and then P.Len = 10 and then
         P.Control_byte.Compound_flag
      then
         P.Control_byte.Compound_flag := False;
      end if;
      ------------------------------------------

      --  Has children?
      if P.Control_byte.Compound_flag and then P.Len > 0 then
         loop
            --  New child
            From_stream (Stream, P_child, Child_size);
            --  Is end?
            if P_child /= null then
               Children_size := Children_size + Child_size;
               --  Check ending without 0 byte:
               if Children_size = P.Len then
                  Add_child (P, P_child);
                  exit;
               elsif Children_size > P.Len then
                  Trace.Log("G2.Packet.Parsing.From_stream (1): " &
                     "Children too large.", Trace.Warning);
                  raise Parse_Error;
               else
                  Add_child (P, P_child);
               end if;
            else
               --  Regular exit because of null byte.
               --  We must count it!
               Children_size := Children_size + 1;
               exit;
            end if;
         end loop;
      end if;

      --  Has payload?
      if Children_size < P.Len then
         declare
            Payload : String (1 .. P.Len - Children_size);
         begin
            String'Read (Stream, Payload);
            P.Payload := U (Payload);
            Read      := Read + Payload'Length;
         end;
      end if;

--      Trace.Log (" <------ ");

      Read  := Read + Children_size;
      Child := P;

   exception
      when E: others =>
         Trace.Log("G2.Packet.Parsing.From_stream (1): " & Trace.Report(E),
            Trace.Warning);

         --  No leaks:
         if P_child /= null then
            Trace.Log ("Child: " & S (P_child.Type_Name) &
            "; Big endian: " & P_child.Control_byte.Big_endian'Img &
            "; Len: " & P_child.Len'Img &
            "; LenLen: " & P_child.Control_byte.Len_Len'Img &
            "; CF: " & P_child.Control_byte.Compound_flag'Img &
            "; NameLen: " & P_child.Control_byte.Name_Len'Img, Trace.Error);
            Free (P_child);
         end if;
         if P /= null then
            Trace.Log ("Packet: " & S (P.Type_Name) &
            "; Big endian: " & P.Control_byte.Big_endian'Img &
            "; Len: " & P.Len'Img &
            "; LenLen: " & P.Control_byte.Len_Len'Img &
            "; CF: " & P.Control_byte.Compound_flag'Img &
            "; NameLen: " & P.Control_byte.Name_Len'Img, Trace.Error);
            Free (P);
         end if;

         Child := null;
         raise;
   end From_stream;

   --  Returns a G2 packet.
   --  It is read fully from the beggining of the stream, assuming there is
   --  enough data in it.
   --  Control byte is also taken from the string; Length as well.
   procedure From_stream (
      Stream       : access Ada.Streams.Root_stream_type'Class;
      Child        : out Packet.Child_access;
      Read         : out Natural)
   is

      Byte         : Unsigned_8;
      Control_byte : Control_byte_type;
      Child_size   : Natural;

   begin

      Read := 0;

      --  Read the control byte:
      Unsigned_8'Read(Stream, Byte);
      Read := Read + 1;


      --  If its null, return null packet (no more children)
      if Byte = 0 then
         Child := null;
         return;
      else
         Control_byte := To_control_byte (Byte);
      end if;

      --  Read length:
      declare
         Len : Endian.Byte_array (1 .. Control_byte.Len_len);
      begin
         Endian.Byte_array'Read (Stream, Len);
         Read := Read + Len'Length;

         --  Read remainder from stream:
         From_stream (
            Control_byte,
            Endian.Convert (Len, Control_byte.Big_endian),
            Stream,
            Child,
            Child_size);
         Read := Read + Child_size;
         return;
      end;

   exception
      when E: others =>
         Trace.Log("G2.Packet.Parsing.From_stream (2): " & Trace.Report(E),
            Trace.Warning);

         raise;
         Child := null;
   end From_stream;

   --  Draws a tree of the packet:
   procedure Trace_tree (
      this : in Packet.Object;
      Level  : in Trace.Warning_Levels := Trace.Debug;
      Indent : in Natural := 0) is
   begin
      Trace_tree (ref (this), Level, Indent);
   end Trace_tree;

   --  Draws a tree of the packet:
   procedure Trace_tree (
      this : in Packet.Child_access;
      Level  : in Trace.Warning_Levels := Trace.Debug;
      Indent : in Natural := 0) is

      Line   : constant String (1 .. Indent) := (others => '-');

   begin
      Trace.Log (Line & S (this.Type_name) &
         " (Payload length:" & Natural'Image (ASU.Length (This.Payload)) &
         ") " & To_hex (S (This.Payload)), Level);
      for n in 1 .. Children_vector.Length (this.Children) loop
         Trace_tree (this.Children.Vector (n), Level, Indent + 2);
      end loop;
   end Trace_tree;

   --  Initialize:
   procedure Create (
      This         : out Object;
      Link         : in  Agpl.Streams.Stream_access;
      Available    : in  Available_function) is
   begin
      this.Link         := Link;
      this.Pipe_status  := Ready;
      this.Packet_len   := 0;
      this.Available    := Available;
   end Create;

   --  Call this function each time a packet is to be checked
   --  Will return Null_packet until a packet is fully acquired:
   procedure Check (
      this              : in out Object;
      Aggresive         : in Boolean := True;
      Result            : out Packet.Object) is

      Available : Natural;
   begin
      --  A priori:
      Result := Null_packet;

      case this.Pipe_status is

         when Ready =>
            --  Read control byte:
            if This.Available (this.Link) > 0 then
               G2.Packet.Control_byte_type'Read (
                  this.Link, this.Control_byte);
               --  There is length ?
               if this.Control_byte.Len_len = 0 then
                  this.Pipe_status   := Length_done;
                  this.Packet_len := 0;
               else
                  this.Pipe_status   := Control_done;
               end if;

               --  Debug append
               Asu.Append (
                  This.Debug_curr_packet, Character'Val (
                     From_control_byte (this.Control_byte)));

               --  Try length / body:
               if Aggresive then
                  Check (this, Result => Result);
               end if;
            end if;

         when Control_done =>
            --  Read length
            if This.Available (this.Link) >=
               this.Control_byte.Len_len
            then
               declare
                  Len : Endian.Byte_array (1 .. this.Control_byte.Len_len);
               begin
                  Endian.Byte_array'Read (this.Link, Len);
                  this.Packet_len := Endian.Convert (Len, this.Control_byte.Big_endian);

                  --  Debug append
                  for N in Len'Range loop
                     Asu.Append (This.Debug_curr_packet, Character'Val (Len (N)));
                  end loop;

                  if this.Packet_len <= Max_packet_size then
                     this.Pipe_status := Length_done;
                  elsif this.Packet_len >= Max_admisible_size then
                     raise Max_admisible_size_error;
                  else
                     this.Pipe_status := Skipping;
                     --  Adjust size to skip:
                     This.Packet_len:=
                        This.Packet_len + This.Control_byte.Name_len + 1;
                     Trace.Log ("G2.Packet.Parsing.Check: Starting to drop packet " &
                        " too large (" & To_string (This.Packet_len) &
                        ")", Trace.Warning);
                  end if;
               end;
               --  Try full packet:
               if Aggresive then
                  Check (this, Result => Result);
               end if;
            end if;

         when Length_done =>
            --  Read a full packet:
            if This.Available (this.Link) >=
               this.Packet_len + this.Control_byte.Name_len + 1 then
               --  Parse a packet!
               Result := G2.Packet.Parsing.From_stream (
                  this.Control_byte, this.Packet_len, this.Link);
               this.Pipe_status := Ready;
               This.Debug_prev_packet := This.Debug_curr_packet;
               This.Debug_curr_packet := Null_ustring;
            end if;

         when Skipping =>
            --  Skip packet length:
            Available :=
               Natural'Min (This.Packet_len,
                  Natural'Min (This.Available (This.Link), 1024));
            if Available > 0 then
               declare
                  Buff : Ada.Streams.Stream_element_array (
                     1 .. Ada.Streams.Stream_element_offset (Available));
                  Last : Ada.Streams.Stream_element_offset;
               begin
                  Ada.Streams.Read (This.Link.all, Buff, Last);
                  if Natural (Last) /= Available then
                     raise Parse_Error;
                  end if;
                  This.Packet_len := This.Packet_len - Available;
                  if This.Packet_len = 0 then
                     This.Pipe_status := Ready;
                     This.Debug_prev_packet := This.Debug_curr_packet;
                     This.Debug_curr_packet := Null_ustring;
                  end if;
               end;
            end if;
      end case;
   end Check;

end Agpl.G2.Packet.Parsing;
