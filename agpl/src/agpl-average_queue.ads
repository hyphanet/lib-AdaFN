 

generic
   type Item is digits <>;
package Agpl.Average_queue is

   pragma Preelaborate;

   --  Raised when no elements to average:
   No_data : exception;

   type Object (Size: Positive) is tagged private;
   type Object_access is access all Object'Class;

   --  Add a new item
   --  Will lost olders when size exceeded
   procedure Push (this: in out Object; New_item: in Item);

   --  Returns the average of pushed objects
   --  May raise No_data
   function Average (this: in Object) return Item;

   --  Says if there is no data to average.
   function Is_empty (This : in Object) return Boolean;

   --  Clear contents
   procedure Clear (This : out Object);

private

   type Data_array is array(Positive range <>) of Item;

   type Object (Size: Positive) is tagged record
      Data   : Data_array (1 .. Size);
      Length : Natural  := 0;
      Pos    : Positive := 1;
      Sum    : Item     := 0.0;
      --  Cached sum to avoid computation when requesting average.
   end record;

end Agpl.Average_queue;
