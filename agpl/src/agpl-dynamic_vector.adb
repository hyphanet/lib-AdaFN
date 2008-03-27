 

--  Package for unbounded vectors, integer-indexed

with Ada.Unchecked_Deallocation;

package body Agpl.Dynamic_vector is

   ---------------
   -- Utilities --
   ---------------

   procedure Free is new Unchecked_Deallocation (
      Item_array,
      Item_array_access);

   ------------
   -- Object --
   ------------

   function "=" (L, R : in Object) return Boolean is
   begin
      if L.First /= R.First then
         return False;
      elsif L.Last /= R.Last then
         return False;
      else
         for I in L.First .. L.Last loop
            if L.Vector (I) /= R.Vector (I) then
               return False;
            end if;
         end loop;
         return True;
      end if;
   end "=";

   --  First "attribute"
   --  O (1)
   function First (this : in Object) return Integer is
   begin
      return this.First;
   end First;

   --  Last "attribute"
   --  O (1)
   function Last (this : in Object) return Integer is
   begin
      return this.Last;
   end Last;

   --  Length "attribute"
   --  O (1)
   function Length (this : in Object) return Integer is
   begin
      return this.Last - this.Vector.all'First + 1;
   end Length;

   function Is_Empty (This : in Object) return Boolean is
   begin
      return Length (This) = 0;
   end Is_Empty;

   --  Grows the vector according to the Grow_factor. Should not be necessary
   --    to be used. It's used internally.
   --  O (n)
   procedure Grow (this : in out Object; Empty_side : Sides := Ending) is
   begin
      declare
         Increment  : constant Natural           :=
            Natural'Max
              (1,
               Natural (Float (this.Vector.all'Length) * Grow_factor));
         New_vector : constant Item_array_access :=
            new Item_array (
            this.Vector.all'First .. this.Vector.all'Last + Increment);
      begin
         --  Assign values:
         if Empty_side = Ending then
            New_vector (New_vector'First .. this.Last) :=
              this.Vector (this.Vector.all'First .. this.Last);
         else
            New_vector (New_vector'First + 1 .. this.Last + 1) :=
              this.Vector (this.Vector.all'First .. this.Last);
         end if;
         --  Replace:
         Free (this.Vector);
         this.Vector := New_vector;
      end;
   end Grow;

   --  Adds an item to the end. Will grow the vector if necessary. O (1) or O
   --  (n) if growing occurs
   procedure Append (this : in out Object; Item : in Item_type) is
   begin
      if this.Last = this.Vector.all'Last then
         --  grow it!
         Grow (this);
         --  Assign value
         this.Last               := this.Last + 1;
         this.Vector (this.Last) := Item;
      else
         this.Last               := this.Last + 1;
         this.Vector (this.Last) := Item;
      end if;
   end Append;

   --  Adds items to the end. Will grow the vector if necessary.
   --  O (1) or O (n) if growing occurs.
   procedure Append_Array (This : in out Object; Items : in Item_Array) is
   begin
      for I in Items'Range loop
         Append (This, Items (I));
      end loop;
   end Append_Array;

   --  Adds an item before a certain position (that could not exist if we
   --    want insertion at Last + 1, i.e., the end. Will grow the vector
   --    if necessary.
   --  O (n)
   procedure Insert
     (this : in out Object;
      Item : in Item_type;
      Pos  : in Integer)
   is
   begin
      if Pos > this.Last or else Pos < this.First then
         raise Constraint_Error;
      end if;

      if this.Last = this.Vector.all'Last then
         --  grow it!
         Grow (this);
      end if;
      --  Slide the values:
      if Pos <= this.Last then
         this.Vector (Pos + 1 .. this.Last + 1) :=
           this.Vector (Pos .. this.Last);
      end if;
      --  Assign value
      this.Last         := this.Last + 1;
      this.Vector (Pos) := Item;
   end Insert;

   --  Deletes an item at given pos O (n)
   procedure Delete (this : in out Object; Pos : in Integer) is
   begin
      if Pos > this.Last or else Pos < this.First then
         raise Constraint_Error;
      end if;
      --  Reassign:
      if Pos /= this.Last then
         this.Vector (Pos .. this.Last - 1) :=
           this.Vector (Pos + 1 .. this.Last);
      end if;
      --  Shrink:
      this.Last := this.Last - 1;
   end Delete;

   --  Delete all ocurrences of an item O (n^2)
   procedure Delete_item (this : in out Object; Item : in Item_type) is
   begin
      for N in reverse  this.First .. this.Last loop
         if this.Vector (N) = Item then
            Delete (this, N);
         end if;
      end loop;
   end Delete_item;

   --  Clean the vector, starts afresh O (1)
   procedure Reset (this : in out Object) is
   begin
      Finalize (Proto_object (this));
      Initialize (Proto_object (this));
      this.Last := this.First - 1;
   end Reset;

   --  Optimize memory usage, vector of only valid positions Right after
   --  optimize, 'Last is valid. O (n)
   procedure Purge (this : in out Object) is
      New_vector : constant Item_array_access :=
         new Item_array (this.First .. this.Last);
   begin
      --  Copy:
      New_vector (this.First .. this.Last) :=
        this.Vector (this.First .. this.Last);
      --  Free old:
      Free (this.Vector);
      --  Replace:
      this.Vector := New_vector;
   end Purge;

   --  Member functions, not very useful if you access the vector directly:
   function Value (this : in Object) return Item_array is
   begin
      return this.Vector.all;
   end Value;

   function Value (this : in Object; Pos : in Integer) return Item_type is
   begin
      return this.Vector (Pos);
   end Value;

   --  Basic searching:
   --  Raise Item_not_found
   --  O (n)
   function Pos
     (this : in Object;
      Item : in Item_type;
      Pos  : in Integer := Integer'First)
      return Integer
   is
   begin
      for N in  Integer'Max (Pos, this.First - 1) + 1 .. this.Last loop
         if this.Vector (N) = Item then
            return N;
         end if;
      end loop;
      raise Item_not_found;
      return 0;
   end Pos;

   procedure Initialize (this : in out Proto_object) is
   begin
      if this.Vector /= null then
         Free (this.Vector);
      end if;

      this.Vector :=
        new Item_array (this.First .. this.First + Initial_size - 1);
   end Initialize;

   procedure Adjust (this : in out Proto_object) is
   begin
      this.Vector := new Item_array'(this.Vector.all);
   end Adjust;

   procedure Finalize (this : in out Proto_object) is
   begin
      Free (this.Vector);
   end Finalize;

   --  Overriden attributes
   procedure Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Item   : in Object)
   is
   begin
      Natural'Output (Stream, Length (Item));
      for N in  Item.First .. Last (Item) loop
         Item_type'Output (Stream, Item.Vector (N));
      end loop;
   end Write;

   procedure Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Result : out Object)
   is
      Length : constant Natural := Natural'Input (Stream);
   begin
      for N in  1 .. Length loop
         Append (Result, Item_type'Input (Stream));
      end loop;
   end Read;

   function To_Vector (This : in Item_Array) return Object is
      Result : Object (First => This'First);
   begin
      for I in This'Range loop
         Append (Result, This (I));
      end loop;

      return Result;
   end To_Vector;

   function To_Array (This : in Object) return Item_Array is
   begin
      return This.Vector (This.Vector'First .. Last (This));
   end To_Array;

end Agpl.Dynamic_vector;
