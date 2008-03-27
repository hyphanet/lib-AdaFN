generic
   type Item (<>)   is limited private; -- Type.
   type Item_access is access Item;     -- This is the access we want safe.
   Item_id : String := "Anonymous";     -- For debug and error reporting.
package Agpl.Smart_Access_Limited_Debug is

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

   function Ref (This : in Object) return Item_Access;
   function "+" (This : in Object) return Item_Access renames Ref;

   procedure Rebind
     (This  : in out Object;
      Data  : in     Item_Access;
      Force : in     Boolean := False);

   procedure Unbind (This : in out Object; Force : in Boolean := False);

private

   type Object is tagged record
      Data : Item_Access;
   end record;

end Agpl.Smart_Access_Limited_Debug;
