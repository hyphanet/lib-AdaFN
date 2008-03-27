

--  This package provides smart access pointers.
--  Thread *safe*.

with Ada.Finalization;  use Ada;

generic
   type Item (<>)   is limited private; -- Type.
   type Item_access is access Item;     -- This is the access we want safe.
   Item_id : String := "Anonymous";     -- For debug and error reporting.
package Agpl.Smart_access_Limited is

   pragma Preelaborate;

   Allocated_Access     : exception;
   --  Raised when trying to bind an already binded access.

   Deallocated_Access   : exception;
   --  Raised when trying to access an already deallocated access.

   Uninitialized_Access : exception;
   --  Raised when trying to access a uninitialized access.

   type Object is tagged private; -- Initially null...

   --  Is null?
   function Is_Null (This : in Object) return Boolean;

   function Is_Valid (This : in Object) return Boolean;
   --  Not null

   --  Initialization.
   --  May raise Allocated_Access.
   procedure Bind (This : in out Object; Data : in Item_Access);
   function  Bind (This : in     Item_Access) return Object;

   --  Get value
   --  Of course, if the access value is copied outside, error can occur.
   --  Since the Item is limited we can't return it
--     function Val (This : in Object) return Item;
--     function "+" (This : in Object) return Item renames Val;

   function Ref (This : in Object) return Item_Access;
   function "+" (This : in Object) return Item_Access renames Ref;

   --  Rebind. Like Unbind but assigning a new value.
   procedure Rebind
     (This  : in out Object;
      Data  : in     Item_Access;
      Force : in     Boolean := False);

   --  Unbinding:
   --  The value is no longer controlled
   --  Only valid if one reference (last) or forced.
   --  In other case, constraint error will be raised.
   --  Note that unbinding forcefully a pointer with more references will also
   --  nullify the other instances!
   procedure Unbind (This : in out Object; Force : in Boolean := False);
   pragma Inline (Unbind);

--     --  Serialization...
--     function Input (S : access Ada.Streams.Root_Stream_Type'Class) return Object;
--     for Object'Input use Input;
--
--     procedure Output (S    : access Ada.Streams.Root_Stream_Type'Class;
--                       This : in Object);
--     for Object'Output use Output;

private

   --  Internal, debugging exceptions:
   Tracker_Already_Deallocated : exception;

   pragma Inline (Is_Null, Ref);

   --  Auxiliary type to do the counting.
   protected type Tracker_Type is
      procedure Discount;
      function Get_Count return Natural;
      function Get_Data  return Item_Access;
      procedure Rebind_Data (This : in Item_Access; Force : in Boolean);
      procedure Set_Data (This : in Item_Access);
      procedure Unbind (Force : in Boolean);

      procedure Add (I : in Integer);
      procedure Free;
   private
      Data  : Item_access;
      Count : Natural     := 1;
      Alloc : Boolean     := False; -- To keep track of unalloc/dealloc status.
   end Tracker_Type;

   type Tracker_access is access Tracker_type;

   procedure Discount (This : in out Tracker_access);

   --  The tracker is always allocated, even for null handlers, for simplicity
   --  and speed reasons.
   type Object is new Finalization.Controlled with record
      Tracker : Tracker_access := new Tracker_Type;
      pragma Atomic (Tracker);
   end record;

   procedure Adjust   (this : in out Object);
   procedure Finalize (this : in out Object);
   pragma Inline (Adjust, Finalize);

end Agpl.Smart_access_Limited;
