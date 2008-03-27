 

--  A bag is a container for unordered objects.
--  Its interesting properties are:
--  O (1) access to any position (though you can't know what's in that position,
--   but this can be useful in randomized algorithms).
--  O (1) insertion and deletion of objects in the bag.
--  Direct access to the array of objects
--   (Take care of using the Last function and not the 'Last attribute)

with Agpl.Dynamic_Vector;

generic
   type Item_type   is private;
   type Bag_Context is private;  -- Bag-specific info
   Initial_size : Natural := 16; -- Use something meaningful to your app.
   Grow_factor  : Float   := 1.5;
package Agpl.Bag is

   pragma Preelaborate;

   package Bag_Vectors is new Agpl.Dynamic_Vector (Item_Type,
                                                   Initial_Size,
                                                   Grow_Factor);

   ------------
   -- Object --
   ------------

   --  Check the Dynamic_Vector package for usage and operations
   type Object is new Bag_Vectors.Object with private;

   function Get_Context (This : in Object) return Bag_Context;
   procedure Set_Context (This : in out Object; Context : in Bag_Context);
   --  The name that will be passed around in the moving callbacks

   --  Adds an item before a certain position (that could not exist if we
   --    want insertion at Last + 1, i.e., the end. Will grow the vector
   --    if necessary.
   --  O (1) or O (n) if growing occurs
   --  Causes the element at the given position to go to the last position
   overriding
   procedure Insert (This  : in out Object;
                     Item  : in     Item_type;
                     Pos   : in     Integer);

   not overriding
   procedure Insert (This  : in out Object;
                     Item  : in     Item_type;
                     Pos   : in     Integer;
                     Moving : access procedure (Item    : in out Item_Type;
                                                Bag     : in out Bag_Context;
                                                Old_Pos,
                                                New_Pos : in     Integer));
   --  This version will call Moving with the moved Item and its indexes of
   --  interest.
   --  Don't alter the bag from within Moving!!

   --  Deletes an item at given pos
   --  O (1)
   --  Causes the last element to occupy the deleted position.
   overriding
   procedure Delete (This  : in out Object;
                     Pos   : in Integer);

   not overriding
   procedure Delete (This  : in out Object;
                     Pos   : in Integer;
                     Moving : access procedure (Item    : in out Item_Type;
                                                Bag     : in out Bag_Context;
                                                Old_Pos,
                                                New_Pos : in     Integer));
   --  Don't alter the bag from within Moving!!

   function To_Bag    (This : in Bag_Vectors.Item_Array) return Object;

private

   type Object is new Bag_Vectors.Object with record
      Busy    : Boolean := False;
      Context : Bag_Context;
   end record;

   function To_Vector (This : in Bag_Vectors.Item_Array) return Object
     renames To_Bag;

end Agpl.Bag;
