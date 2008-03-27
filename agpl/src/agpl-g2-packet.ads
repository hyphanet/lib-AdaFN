 

--  Binary packetized representation of XML-like data. Taken from G2 network
--  packet format.

--  Note that this G2 implementation violates the G2 draft in the sense that
--  each child takes its endianness from its control parent, not the topmost
--  packet.
--  This lead to erroneous results for packets with mixed endianness.

with Agpl.Dynamic_vector;
with Agpl.Smart_Access_Limited;
pragma Elaborate_All (Agpl.Smart_Access_Limited);
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Ada.Calendar;
with Ada.Finalization;
with Ada.Streams;
with Ada.Strings;          use Ada.Strings;
with Ada.Strings.Bounded;
use Ada;

with System;

package Agpl.G2.Packet is

   Parse_Error  : exception;

   --  We'll allow a max of children for any packet:
   MAX_CHILDREN : constant := 2048;

   --  Max packet size in bytes.
   Max_packet_size : Natural := 256 * 1024;

   --  Absolute max. Packets over this size will cause connection drop
   Max_admisible_size       : Natural := 1024 * 1024;
   Max_admisible_size_error : exception;

   type Object is tagged private;
   --  Corresponds with a packet.

   type Object_array is array (Integer range <>) of Object;

                                       --  Create a packet with given name and payload:
   --  Returns an allocated object
   function Create (Name : in String; Payload : in String := "")
      return Object;

   --  Makes an object into child of another one.
   --  May raise exception if too many childs
   --  Doesn't check for duplicates.
   --  Check null additions (no effect).
   --  Check for /TO child, to put it the first.
   --  The child packet is set to NULL!
   procedure Add_child (
      Parent    : in Object;
      New_child : in out Object);
   pragma Inline (Add_child);

   --  Returns a child as an object
   --  Will raise Constraint_error if that child appears multiple times
   --  Name is in the form "xx/yy/zz"
   --  Null_packet returned if it doesn't exists.
   function Get_child (this : in Object; Name : in String) return Object;

   --  Get children of a given type. Inmediate depth only.
   --  Will raise Constraint_Error if more than MAX_CHILDREN
   function Get_children (this : in Object; Name : in String)
      return Object_array;

   --  Root name of a packet:
   function Name (this : in Object) return String;
   pragma Inline (Name);

   --  Root payload as a string:
   function Payload (this : in Object) return String;
   pragma Inline (Payload);

   --  Arrival time:
   function Arrival_time (this : in Object) return Calendar.Time;
   pragma Inline (Arrival_time);

   --  Big endian?
   function Big_endian (this: in Object) return Boolean;
   pragma Inline (Big_endian);

   --  Returns the expected full length (control + len_len + name_len + etc)
   function Full_length (this : in Object) return Natural;

   --  Hex representation of a packet:
   function To_hex (this : in Object; Interleaving : String := " ")
      return String;

   --  Enumeration of children in a packet:
   function To_Text (This : in Object; Show_Payloads : Boolean := False) return String;

   --  Is_a: says if a packet qualifies for some kind.
   --  Should have initial / (i.e: /PI/UDP)
   function Is_a (this : in Object; Kind : in String) return Boolean;

   --  Writing to stream.
   procedure Write (Stream : access Streams.Root_stream_type'Class; this : in Object);
   pragma Inline (Write);

   --  Atomic writing to a socket stream. It guarantees that the entire
   --    packet is written (or not a byte) in a non-blocking socket stream.
   --  May raise some exception instead of setting success to false.
   procedure Atomic_Write (
      Stream  : access Streams.Root_stream_type'Class;
      This    : in     Object;
      Success : out    Boolean);

   --  Deep copy: Clone an object and all its children
   function Clone (this : in Object) return Object;

   function To_Stream_Element_Array (This : in Object)
                                     return Ada.Streams.Stream_Element_Array;
   --  Raw representation.

private

   --  We'll use a bounded string for the packet type:
   package BStrings is new Bounded.Generic_bounded_length (8);

   function B
     (S : in String;
      Drop : in Truncation := Error) return BStrings.Bounded_string
      renames BStrings.To_Bounded_String;

   function S (B : in BStrings.Bounded_string) return String
      renames BStrings.To_String;

   Nul : BStrings.Bounded_string renames BStrings.Null_bounded_string;

   --  Control byte of all G2 packets:
   type Control_byte_type is record
      Len_len       : Natural range 0 .. 3;
      Name_len      : Natural range 0 .. 7;
      Compound_flag : Boolean                := False;
      Big_endian    : Boolean                := False;
      Reserved      : Integer range 0 .. 1   := 0;
   end record;

   for Control_byte_type'Bit_Order use System.Low_order_first;

   for Control_byte_type use record
      Len_len        at 0 range 6 .. 7;
      Name_len       at 0 range 3 .. 5;
      Compound_flag  at 0 range 2 .. 2;
      Big_endian     at 0 range 1 .. 1;
      Reserved       at 0 range 0 .. 0;
   end record;

   for Control_byte_type'Size use 8;

   --  Serialization of control byte:
   procedure Write (
      Stream       : access Streams.Root_stream_type'Class;
      this         : in     Control_byte_type);
   for Control_byte_type'Write use Write;

   procedure Read(
      Stream       : access Streams.Root_stream_type'Class;
      this         :    out Control_byte_type);
   for Control_byte_type'Read use Read;

   --  A full G2 packet:
   type Child;
   type Child_access is access all Child;

   package Children_vector is new
     Agpl.Dynamic_vector (Child_access, Initial_Size => 1, Grow_Factor => 2.0);

   type Child is new Finalization.Limited_Controlled with record
      Control_byte : Control_byte_type;
      Len          : Natural range 0 .. 2 ** 24  - 1 := 0;
      Type_name    : BStrings.Bounded_string := Nul;
      Payload      : UString;
      Children     : Children_vector.Object (First => 1);
      Arrival_time : Calendar.Time := Calendar.Clock;
   end record;

   --  Delete a packet:
   procedure Free (this : in out Child_access);

   --  Recursively frees any children
   procedure Finalize (this : in out Child);

   --  Adds a child to a packet:
   --  May raise exception if too many childs
   --  Doesn't check for duplicates.
   --  Check null additions (no effect).
   --  The child packet is set to NULL!
   procedure Add_child (
      Parent    : in Child_access;
      New_child : in out Child_access);
   pragma Inline (Add_child);

   --  Get a given child from an object
   --  Name is in the form "xx/yy/zz"
   --  Must be unique
   function Get_child (
      this : in Child_access;
      Name : in String) return Child_access;

   --  Full size of a packet, including:
   --    control byte, len, name, children, payload.
   --  Only valid for received packets, not created ones.
   function Full_size (this : in Child_access) return Natural;
   pragma Inline (Full_size);

   --  Full size of children packets of a packet:
   --  Only valid for received packets, not created ones.
   function Children_size (this : in Child_access) return Natural;
   pragma Inline (Children_size);

   --  Is_a: says if a packet qualifies for some kind.
   --  Should have initial / (i.e: /PI/UDP)
   function Is_a (this : in Child_access; Kind : in String) return Boolean;
   pragma Inline (Is_a);

   --  Returns the expected length of child + payload
   --  That's the length of CHILDREN + \0 SEPARATOR IF NEEDED + PAYLOAD
   function Computed_length (this : in Child) return Natural;

   --  Returns the expected full length (control + len_len + name_len + etc)
   --  That's the FULL LENGTH OF THIS CHILD, HEADERS PLUS ITS CHILDREN
   function Full_length (this : in Child) return Natural;

   --  Return the number of bytes neccesaries to carry this number:
   function Len_len (N : Natural) return Natural;
   pragma Inline (Len_len);

   --  Writing to stream.
   procedure Write (
      Stream : access Streams.Root_stream_type'Class; this : in Child);
   for Child'Write use Write;

   --  Deep copy: Clone a child and all its children
   function Clone (this : in Child_access) return Child_access;

   --  We'll use safe accesses for this thing:
   package Safe_child is new Smart_Access_Limited (Child, Child_access);

   type Object is new Safe_child.Object with null record;

   function Bind (This : in Child_Access) return Object;

   Null_payload : constant UString := Null_Ustring;

end Agpl.G2.Packet;
