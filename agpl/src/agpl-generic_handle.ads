

--  A container for indefinite objects allowing an easier storage for them.
--  'Read and 'Write are implemented, so this type can be safely serialized.

private with Ada.Finalization;
with Ada.Streams;

generic
   type Item (<>) is private;
package Agpl.Generic_Handle is

   pragma Preelaborate;

   type Item_Access is access all Item;

   No_Data : exception;

   type Object is tagged private;

   pragma Preelaborable_Initialization (Object);

   function Null_Object return Object;
   pragma Inline (Null_Object);

   function Set (This : in Item) return Object;
   function "+" (This : in Item) return Object renames Set;
   pragma Inline (Set);
   --  Creation

   function Set (This : in Item_Access) return Object;
   function "+" (This : in Item_Access) return Object renames Set;
   pragma Inline (Set);

   procedure Set (This : in out Object; X : in Item);
   procedure Set (This : in out Object; X : in Item_Access);
   --  Creation

   function Get (This : in Object) return Item;
   function "+" (This : in Object) return Item renames Get;
   function Element (This : in Object) return Item renames Get;
   pragma Inline (Get);
   --  Extraction. May raise No_Data if uninitialized.

   function Ref (This : in Object) return Item_Access;
   pragma Inline (Ref);
   --  Reference to the held item.

   procedure Clear (This : in out Object);
   --  Make it uninitialized.

   function Is_Valid (This : in Object) return Boolean;
   pragma Inline (Is_Valid);
   --  Says if it contains some value.

   function "=" (L, R : Object) return Boolean;

   procedure Read (Stream : access Ada.Streams.Root_Stream_Type'Class;
                   This   :    out Object);
   for Object'Read use Read;

   procedure Write (Stream : access Ada.Streams.Root_Stream_Type'Class;
                    This   : in     Object);
   for Object'Write use Write;

private

   type Object is new Ada.Finalization.Controlled with record
      Data : Item_Access;
   end record;

   procedure Adjust   (This : in out Object);
   procedure Finalize (This : in out Object);

end Agpl.Generic_Handle;
