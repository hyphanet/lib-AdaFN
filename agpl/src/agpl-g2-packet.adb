 

with Agpl.Conversions; use Agpl.Conversions;
with Agpl.Endian;
with Agpl.Streams.Memory_Arrays_Constrained;
with Agpl.Trace;          use Agpl.Trace;

with Ada.Unchecked_conversion;

package body Agpl.G2.Packet is

   use type Agpl.Types.Ustrings.Ustring;

   Null_Packet : Object;

   function Length (this : in Children_vector.Object) return Integer
      renames Children_vector.Length;

   --  Returns the head kind of a packet : Head ("/PI/PO") returns "PI"
   function Head (S : String) return String;
   --  Returns the tail kind of a packet : Head ("/PI/PO") returns "/PO"
   function Tail (S : String) return String;

   --  Create a packet with given name and payload:
   --  Returns an allocated object
   function Create (Name : in String; Payload : in String := "")
      return Object is
      C : constant Child_access := new Child;
      P : Object;
   begin
      C.Type_name             := B (Name);
      C.Control_byte.Name_len := Name'Length - 1;
      C.Payload := U (Payload);

      Bind (P, C);

      return P;
   end Create;

   --  Serialization of that control byte:
   procedure Write (
      Stream       : access Streams.Root_stream_type'Class;
      this         : Control_byte_type) is

      function To_byte is new
         Unchecked_conversion (Control_byte_type, Character);

   begin
      Character'Write (Stream, To_byte (this));
   end Write;

   procedure Read(
      Stream       : access Streams.Root_stream_type'Class;
      this         : out Control_byte_type) is

      function To_control is new
         Unchecked_conversion (Character, Control_byte_type);
      B : Character;

   begin
      Character'Read (Stream, B);
      this := To_control (B);
   end Read;

   --  Recursively frees any children
   procedure Finalize (this : in out Child) is
   begin
      for N in 1 .. Length (this.Children) loop
         Free (this.Children.Vector (N));
      end loop;
   end Finalize;

   --  Delete a packet:
   procedure Free (this : in out Child_access) is
      procedure Delete is new Unchecked_deallocation(Child, Child_access);
   begin
      Delete (this);
   end Free;

   ------------------------------------------------------------------------
   -- Clone                                                              --
   ------------------------------------------------------------------------
   --  Deep copy: Clone a child and all its children
   function Clone (this : in Child_access) return Child_access is
      Result : constant Child_access := new Child;
   begin
      Result.Control_byte := This.Control_byte;
      Result.Len          := This.Len;
      Result.Type_name    := This.Type_name;
      Result.Payload      := This.Payload;
      Result.Arrival_time := This.Arrival_time;
      Result.Children     := This.Children; -- To get the same length
      --  Now clone children
      for N in 1 .. Length (This.Children) loop
         Result.Children.Vector (N) := Clone (This.Children.Vector (N));
      end loop;

      return Result;
   end Clone;

   --  Adds a child to a packet:
   --  May raise exception if too many childs
   procedure Add_child (
      Parent    : in Child_access;
      New_child : in out Child_access) is
   begin
      --  Check null:
      if New_child = null then
         return;
      end if;

      --  Add it:
      begin
         Parent.Control_byte.Compound_flag := True;
         if Length (Parent.Children) = MAX_CHILDREN then
            raise Constraint_Error;
         end if;
         if S (New_child.Type_name) = "TO" then
            Children_vector.Insert (
               Parent.Children, New_child, Parent.Children.Vector'First);
         else
            Children_vector.Append (Parent.Children, New_child);
         end if;
         New_child := null;
      exception
         when Constraint_Error =>
            Trace.Log("G2.Packet.Add_child: Dropping child " &
               "(max reached). Max is" & Integer'Image (MAX_CHILDREN), Trace.Warning);
            Trace.Log("Packet: " & S (Parent.Type_Name), Trace.Warning);
            Free (New_child);
      end;
   end Add_child;

   --  Full size of a packet, including:
   --    control byte, len, name, children, payload.
   function Full_size (this : in Child_access) return Natural is
   begin
      return
         1 +                              -- Control_byte
         this.Control_byte.Len_len +      -- Len
         this.Control_byte.Name_len + 1 + -- Name
         this.Len;                        -- Children + Payload
   end Full_size;

   --  Full size of children packets of a packet:
   function Children_size (this : in Child_access) return Natural is
      Size : Natural := 0;
   begin
      for n in 1 .. Length (this.Children) loop
         Size := Size + Full_size (this.Children.Vector (n));
      end loop;

      return Size;
   end Children_size;

   --  Makes an object into child of another one.
   --  May raise exception if too many childs
   --  Doesn't check for duplicates.
   --  Check null additions (no effect).
   procedure Add_child (Parent: in Object; New_child : in out Object) is
      C : Child_access := +New_child;
   begin
      --  Prevent releasing:
      Unbind (New_child);
      New_child := Null_packet;
      --  Add normally:
      Add_child (+Parent, C);
   end Add_child;

   --  Root name of a packet:
   function Name (this : in Object) return String is
   begin
      return BStrings.To_string (Ref (this).Type_name);
   end Name;

   --  Root payload as a string:
   function Payload (this : in Object) return String is
   begin
      return S (Ref (this).Payload);
   end Payload;

   --  Arrival time:
   function Arrival_time (this : in Object) return Calendar.Time is
   begin
      return Ref (this).Arrival_time;
   end Arrival_time;

   --  Big endian?
   function Big_endian (this: in Object) return Boolean is
   begin
      return Ref (this).Control_byte.Big_endian;
   end Big_endian;

   --  Hex representation of a packet:
   function To_hex (this : in Object; Interleaving : String := " ")
      return String is

      function To_char is new Unchecked_conversion
        (Control_byte_type, Character);

      Result : UString;

      C : constant Child_access := Ref (this);

   begin

      --  Control byte:
      Result := U (To_hex (To_char (C.Control_byte))) & Interleaving;

      --  Length:
      Result := Result & To_string (C.Len) & Interleaving;

      --  Name:
      Result := Result & S (C.Type_name) & Interleaving;

      --  Payload:
      declare
         Payload : constant String := S (C.Payload);
      begin
         for n in Payload'Range loop
            Result := Result & To_hex(Payload (n));
         end loop;
      end;

      return S (Result);

   end To_hex;

   --  Enumeration of children in a packet:
   function To_Text (
      This : in Object; Show_Payloads : in Boolean := False) return String
   is

      function To_Text (This : in Child_Access; Show_Payloads : in Boolean) return String
      is
         Line : Ustring := U (S (This.Type_Name));
      begin
         if Show_Payloads then
            Asu.Append (Line, ":");
            Asu.Append (Line, This.Payload);
         end if;
         for n in 1 .. Children_vector.Length (this.Children) loop
            Asu.Append (Line, "->");
            Asu.Append (Line, To_Text (this.Children.Vector (n), Show_Payloads));
         end loop;
         Asu.Append (Line, ";");

         return S (Line);
      end To_Text;

   begin
      return To_Text (Ref (This), Show_Payloads);
   end To_Text;

   --  Is_a: says if a packet qualifies for some kind.
   --  Should have initial / (i.e: /PI/UDP)
   function Is_a (this : in Object; Kind : in String) return Boolean is
   begin
      return Is_a (Ref (this), Kind);
   end Is_a;

   --  Is_a: says if a packet qualifies for some kind.
   --  Should have initial / (i.e: /PI/UDP)
   function Is_a (this : in Child_access; Kind : in String) return Boolean is
   begin
      if Kind = "" then
         return True;
      end if;
      if Head (Kind) /= S (this.Type_name) then
         return False;
      end if;
      --  Check all children
      declare
         T : constant String := Tail (Kind);
      begin
         if T = "" then
            return True;
         else
            for N in 1 .. Length (this.Children) loop
               if Is_a (this.Children.Vector (N), T) then
                  return True;
               end if;
            end loop;
            --  Not found, hence:
            return False;
         end if;
      end;
   end Is_a;

   --  Get a given child from an object
   --  Name is in the form "xx/yy/zz"
   --  Must be unique
   function Get_child (this : in Child_access; Name : in String)
      return Child_access is
      H     : constant String := Head ("/" & Name);
      T     : constant String := Tail (Name);
      Found : Boolean := False;
      C     : Child_access;
   begin
      --  Search current one:
      for N in 1 .. Length (this.Children) loop
         if S (this.Children.Vector (N).Type_name) = H then
            C := this.Children.Vector (N);
            if Found then
               raise Constraint_Error;
            else
               Found := True;
            end if;
         end if;
      end loop;

      --  Null if not found
      if C = null then
         return null;
      end if;

      if T = "" then
         return C;
      else
         return Get_child (C, T (T'First + 1 .. T'Last));
      end if;
   end Get_child;

   --  Returns a child as an object
   --  Will raise Constraint_error if that child appears multiple times
   --  Name is in the form "xx/yy/zz"
   function Get_child (this : in Object; Name : in String) return Object is
      C      : Child_access;
      Result : Object;
   begin
      C := Get_Child (Ref (this), Name);
      if C = null then
         Result := Null_packet;
      else
         --  Duplicate it to not have safe and unsafe refs to the same child:
         Bind (Result, Clone (C));
      end if;

      return Result;
   end Get_Child;

   ------------------------------------------------------------------------
   -- Get_children                                                       --
   ------------------------------------------------------------------------
   --  Get children of a given type. Inmediate depth only.
   function Get_children (this : in Object; Name : in String)
   return Object_array is
      R : Object_Array (1 .. MAX_CHILDREN);
      N : Natural := 0;
      C : Object;
   begin
      for I in 1 .. Length (Ref (this).Children) loop
         if S (Ref (this).Children.Vector (I).Type_name) = Name then
            Bind (C, Clone (Ref (this).Children.Vector (I)));
            N     := N + 1;
            R (N) := C; -- May raise Constraint_Error
         end if;
      end loop;
      return R (R'First .. N);
   end Get_children;

   --  Returns the expected length of child + payload
   --  That's the length of CHILDREN + \0 SEPARATOR IF NEEDED + PAYLOAD
   function Computed_length (this : in Child) return Natural is
      Result : Natural := 0;
      Len    : constant Natural := Length (this.Children); --  The number of children,
                                                           --  not its size.
   begin
      --  Simple packet
      if Len = 0 and then this.Payload = Null_payload then
         return 0;
      elsif Len > 0 and then this.Payload = Null_payload then
      --  Only children, we can adjust the size without trailing \0
         for N in 1 .. Len loop
            Result := Result + Full_length (this.Children.Vector (N).all);
         end loop;
      elsif Len = 0 and then this.Payload /= Null_payload then
         return ASU.Length (this.Payload);
      else
      --  Children plus payload:
         for N in 1 .. Len loop
            Result := Result + Full_length (this.Children.Vector (N).all);
         end loop;
         Result := Result + 1;            -- The \0 marker for end-of-childs
         Result := Result + ASU.Length (this.Payload);   -- Payload
      end if;

      return Result;
   end Computed_length;

   --  Returns the expected full length (control + len_len + name_len + etc)
   --  That's the FULL LENGTH OF THIS CHILD, HEADERS PLUS ITS CHILDREN
   function Full_length (this : in Child) return Natural is
   begin
      return
         1 +
         Len_len (Computed_length (this)) +
         Computed_length (this) +
         this.Control_byte.Name_len + 1;
   end Full_length;

   --  Returns the expected full length (control + len_len + name_len + etc)
   function Full_length (this : in Object) return Natural is
   begin
      return Full_length (Ref (This).all);
   end Full_length;

   --  Return the number of bytes neccesaries to carry this number:
   function Len_len (N : Natural) return Natural is
   begin
      if N = 0 then
         return 0;
      elsif N < 2 ** 8 then
         return 1;
      elsif N < 2 ** 16 then
         return 2;
      else
         return 3;
      end if;
   end Len_Len;

   --  Writing to stream.
   procedure Write (
      Stream : access Streams.Root_stream_type'Class; this : in Object) is
   begin
      Write (Stream, Ref (this).all);
   end Write;

   --  Writing to stream.
   procedure Write (
      Stream : access Streams.Root_stream_type'Class; this : in Child) is

      Control_byte : Control_byte_type := this.Control_byte;
      Length       : constant Natural := Computed_length (this);
      Length_array : constant Endian.Byte_array :=
                       Endian.Convert (Length, Control_byte.Big_endian);
         --  Preserve endianness, in case payload needs it.

   begin
      Control_byte.Len_len := Length_array'Length;
      --  Watch to not send a \0:
      if Control_byte.Len_len = 0 then
         Control_byte.Compound_flag := True;
      end if;

      --  Send control_byte
      Control_byte_type'Write (Stream, Control_byte);
      --  Send length (maybe an empty array, so nothing is sent).
      Endian.Byte_array'Write (Stream, Length_array);
      --  Send name
      String'Write (Stream, S (this.Type_name));
      --  Send childs
      for N in 1 .. Children_vector.Length (this.Children) loop
         Write (Stream, this.Children.Vector (N).all);
      end loop;
      --  Send \0 if needed
      if Control_byte.Compound_flag and then this.Payload /= Null_payload then
         Endian.Byte'Write (Stream, 0);
      end if;
      --  Send payload;
      if this.Payload /= Null_payload then
         String'Write (Stream, S (this.Payload));
      end if;
   end Write;

   --  Atomic writing to a socket stream. It guarantees that the entire
   --    packet is written (or not a byte) in a non-blocking socket stream.
   procedure Atomic_Write (
      Stream  : access Streams.Root_stream_type'Class;
      This    : in     Object;
      Success : out    Boolean)
   is
      use Streams;
      package Mem_Stream is new
        Agpl.Streams.Memory_Arrays_Constrained
          (Stream_Element_Offset (Full_Length (This)));

      Buffer  : aliased Mem_Stream.Sized_Array;
      BStream : aliased Mem_Stream.Stream_type (Buffer'Access);
   begin
      Write (Streams.Root_stream_type'Class (Bstream)'Access, This);
      Ada.Streams.Write (Stream.all, Buffer);
      Success := True;
   end Atomic_write;

   --  Deep copy: Clone an object and all its children
   function Clone (this : in Object) return Object is
      O : Object;
   begin
      Bind (O, Clone (Ref (this)));

      return O;
   end Clone;

   function To_Stream_Element_Array (This : in Object)
                                     return Ada.Streams.Stream_Element_Array
   is
      use Agpl.Streams;
      package Mem_Stream is new
        Agpl.Streams.Memory_Arrays_Constrained
          (Stream_Element_Offset (Full_Length (This)));

      Buffer  : aliased Mem_Stream.Sized_Array;
      BStream : aliased Mem_Stream.Stream_type (Buffer'Access);
   begin
      Write (Streams.Root_stream_type'Class (Bstream)'Access, This);
      return Buffer;
   end To_Stream_Element_Array;

   ---------------
   -- Utilities --
   ---------------
   --  Returns the head kind of a packet : Head ("/PI/PO") returns "PI"
   function Head (S : String) return String is
   begin
      for N in S'First + 1 .. S'Last loop
         if S (N) = '/' then
            return S (S'First + 1 .. N - 1);
         end if;
      end loop;

      return S (S'First + 1 .. S'Last);
   end Head;

   --  Returns the tail kind of a packet : Head ("/PI/PO") returns "/PO"
   function Tail (S : String) return String is
   begin
      for N in S'First + 1 .. S'Last loop
         if S (N) = '/' then
            return S (N .. S'Last);
         end if;
      end loop;

      return "";
   end Tail;

   function Bind (This : in Child_Access) return Object is
   begin
      return (Safe_Child.Bind (This) with null record);
   end Bind;

begin
--   Statistics.Object.Set (
--      Stat_num_children, Statistics.Integers.Create (0));
   null;
end Agpl.G2.Packet;
