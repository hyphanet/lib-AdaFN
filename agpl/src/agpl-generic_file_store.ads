

generic
   type Item is private;
package Agpl.Generic_File_Store is

   type Item_Access is access all Item;

   procedure To_File (This : in Item; File : in String);
   --  This uses Item'Write
   --  Write a file with the dumped item. Overwrites if existing.

   function Load (File : in String) return Item;
   --  This uses Item'Read

   procedure Load (This : out Item; File : in String);
   --  This uses Item'Read
   --  In place to avoid extra copy at return.

end Agpl.Generic_File_Store;
