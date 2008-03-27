 

generic
   type Item (<>) is private;
package Agpl.Generic_Indefinite_File_Store is

   pragma Elaborate_Body;

   type Item_Access is access all Item;

   procedure To_File (This : in Item; File : in String);
   --  This uses Item'Output
   --  Write a file with the dumped item. Overwrites if existing.

   function Load (File : in String) return Item;
   --  This uses Item'Input

end Agpl.Generic_Indefinite_File_Store;
