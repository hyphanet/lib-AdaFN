 

--  Package for unbounded vectors, integer-indexed

with Ada.Finalization;  use Ada;
with Ada.Streams;

generic
   type Item_type is private;
   Initial_size : Natural := 16;
   Grow_factor  : Float   := 1.5;
package Agpl.Dynamic_vector is

   pragma Preelaborate;

   Item_not_found : exception;

   type Item_array is array (Integer range <>) of Item_type;
   type Item_array_access is access all Item_array;

   --  That's visible so we can access the internal array an mimic the usual
   --  arrays semantics except for 'Last attribute: Last function must be used.
   --  First can be anything except Integer'First
   --  Controlling guarantees that no leaks occur, and that copies are deep,
   --    and that vector.all is valid from the very first moment.
   type Proto_object (First : Integer) is new Finalization.Controlled with
      record
         Vector : Item_array_access;
      end record;

   ------------
   -- Object --
   ------------

   --  First is the index for the first element.
   type Object (First : Integer) is new Proto_object with private;

   function "=" (L, R : in Object) return Boolean;

   --  First "attribute"
   --  O (1)
   function First (this : in Object) return Integer;
   pragma Inline (First);

   --  Last "attribute"
   --  O (1)
   function Last (this : in Object) return Integer;
   pragma Inline (Last);

   --  Length "attribute"
   --  O (1)
   function Length (this : in Object) return Integer;
   pragma Inline (Length);

   --  O (1)
   function Is_Empty (This : in Object) return Boolean;
   pragma Inline (Is_Empty);

   --  Adds an item to the end. Will grow the vector if necessary.
   --  O (1) or O (n) if growing occurs.
   procedure Append (this : in out Object; Item : in Item_type);

   --  Adds items to the end. Will grow the vector if necessary.
   --  O (1) or O (n) if growing occurs.
   procedure Append_Array (This : in out Object; Items : in Item_Array);

   --  Adds an item before a certain position (that could not exist if we
   --    want insertion at Last + 1, i.e., the end. Will grow the vector
   --    if necessary.
   --  O (n)
   procedure Insert (
      this : in out Object; Item : in Item_type; Pos : in Integer);

   --  Grows the vector according to the Grow_factor. Should not be necessary
   --    to be used. It's used internally.
   --  O (n)
   type Sides is (Start, Ending);
   procedure Grow (this : in out Object; Empty_side : in Sides := Ending);

   --  Deletes an item at given pos
   --  O (n)
   procedure Delete (this : in out Object; Pos : in Integer);

   --  Delete all ocurrences of an item
   --  O (n^2)
   procedure Delete_item (this : in out Object; Item : in Item_type);

   --  Clean the vector, starts afresh
   --  O (1)
   procedure Reset (this : in out Object);
   procedure Clear (this : in out Object) renames Reset;

   --  Optimize memory usage, vector of only valid positions
   --  Right after optimize, 'Last is valid.
   --  O (n)
   procedure Purge (this : in out Object);

   --  Member functions, not very useful if you access the vector directly:
   function Value (this : in Object) return Item_array;
   pragma Inline (Value);

   function Value (this : in Object; Pos : in Integer) return Item_type;
   pragma Inline (Value);

   --  Basic searching:
   --  Returns first found after given position
   --  Raise Item_not_found
   --  O (n)
   function Pos (
      this : in Object;
      Item : in Item_type;
      Pos  : in Integer := Integer'First) return Integer;

   --  Overriden attributes
   procedure Write (
      Stream : access Ada.Streams.Root_stream_type'Class;
      Item   : in Object);
   for Object'Write use Write;

   procedure Read (
      Stream : access Ada.Streams.Root_stream_type'Class;
      Result : out Object);
   for Object'Read use Read;

   --  Conversion utilities
   function To_Vector (This : in Item_Array) return Object;

   function To_Array (This : in Object) return Item_Array;

private

   procedure Initialize (this : in out Proto_object);
   procedure Adjust     (this : in out Proto_object);
   procedure Finalize   (this : in out Proto_object);

   type Object (First : Integer) is new Proto_object (First) with record
      Last : Integer := First - 1;
   end record;

end Agpl.Dynamic_vector;
