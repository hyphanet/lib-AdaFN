 

--  Implements a double indexed container, with sorted semantics and
--  search by id.
--  However, for the search by Id to work, the ordering key must be the same too

with Ada.Containers.Ordered_Multisets;

generic
   type Element_type is private;
   with function "<" (
      Left : Element_type; Right : Element_type) return Boolean is <>;
   --  Orders Elements; usually will work on a part (the key) of an Element
   --  Elements will be ordered in ascending order according to "<"
   with function "=" (
      Left : Element_type; Right : Element_type) return Boolean is <>;
   --  Usually operates on part (the key) of an Element
package Agpl.Protected_sorted_index is

   pragma Preelaborate;

   package Implementation is new Ada.Containers.Ordered_Multisets (
      Element_type, "<", "=");

   protected type Sorted_index is

      procedure Clear;

      --  No duplicates (replacement)
      procedure Insert (Item : in Element_type);

      function Find (Item : in Element_type) return Boolean;

      --  Success will be true if the item has been found and deleted
      procedure Delete (Item : in Element_type; Success : out Boolean);

      --  Blocking if empty
      --  Doesn't remove it
      entry Get_first (Item : out Element_type);

      --  Get first if possible, without removing it:
      procedure Get_first (Item : out Element_type; Success : out Boolean);

      --  Get and remove an element if found, nothing else.
      procedure Get_remove (Item : in out Element_type; Found : out Boolean);

      --  Get first if exists and remove it
      procedure Get_first_remove (
         Item : out Element_type; Found : out Boolean);

      function Is_empty return Boolean;

      function Length return Natural;

   private

      List : Implementation.Set;

   end Sorted_index;

end Agpl.Protected_sorted_index;
