--  This forces the use of the hash as filename.

generic
   type Index is limited private;
   with function Hash (This : Index) return String;
package Agpl.Generic_File_Store.Cache is

   pragma Elaborate_Body;

   procedure To_File (This : Item;
                      Idx  : Index);
   --  Computes hash and calls the Store.To_File with hash as filename

   function Load (Idx : Index) return Item;
   --  This uses Item'Read

   procedure Load (This : out Item;
                   Idx  :     Index);
   --  This uses Item'Read
   --  In place to avoid extra copy at return.

end Agpl.Generic_File_Store.Cache;
