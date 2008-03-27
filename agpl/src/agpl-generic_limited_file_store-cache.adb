package body Agpl.Generic_Limited_File_Store.Cache is

   -------------
   -- To_File --
   -------------

   procedure To_File
     (This : Item;
      Idx  : Index)
   is
   begin
      To_File (This, Hash (Idx));
   end To_File;

   ----------
   -- Load --
   ----------

   procedure Load
     (This : out Item;
      Idx  :     Index)
   is
   begin
      Load (This, Hash (Idx));
   end Load;

end Agpl.Generic_Limited_File_Store.Cache;
