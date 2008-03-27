 

with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Ada.Finalization;
with Ada.Unchecked_Deallocation;

package Agpl.Strings.Pointers is

   --  To have a String but allocated in heap.

   pragma Preelaborate;

   type String_Access is access all String;

   type Object is tagged private;

   function Create (This : String) return Object;
   function Create (This : Ustring) return Object;

   function Ref (This : Object) return String_Access;

   procedure Free is new Ada.Unchecked_Deallocation (String, String_Access);

private

   type Object is new Ada.Finalization.Controlled with record
      Ptr : String_Access;
   end record;

   procedure Adjust     (This : in out Object);
   procedure Finalize   (This : in out Object);

end Agpl.Strings.Pointers;
