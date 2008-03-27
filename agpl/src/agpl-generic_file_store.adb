with Ada.Streams.Stream_Io;

package body Agpl.Generic_File_Store is

   -------------
   -- To_File --
   -------------

   procedure To_File (This : in Item; File : in String) is
      use Ada.Streams.Stream_Io;
      F : File_Type;
   begin
      Create (F, Name => File, Mode => Out_File);
      Item'Write (Stream (F), This);
      Close (F);
   end To_File;

   ----------
   -- Load --
   ----------

   function Load (File : in String) return Item is
      Result : Item;
   begin
      Load (Result, File);

      return Result;
   end Load;

   ----------
   -- Load --
   ----------

   procedure Load (This : out Item; File : in String) is
      use Ada.Streams.Stream_Io;
      F : File_Type;
   begin
      Open (F, Name => File, Mode => In_File);

      Item'Read (Stream (F), This);

      Close (F);
   end Load;

end Agpl.Generic_File_Store;
