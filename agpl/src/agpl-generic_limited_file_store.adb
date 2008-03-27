with Ada.Streams.Stream_Io;

package body Agpl.Generic_Limited_File_Store is

   -------------
   -- To_File --
   -------------

   procedure To_File (This : in Item; File : in String) is
      use Ada.Streams.Stream_Io;
      F : File_Type;
   begin
      Create (F, Name => File, Mode => Out_File);
      Write (Stream (F), This);
      Close (F);
   end To_File;

   ----------
   -- Load --
   ----------

   procedure Load (This : out Item; File : in String) is
      use Ada.Streams.Stream_Io;
      F : File_Type;
   begin
      Open (F, Name => File, Mode => In_File);

      Read (Stream (F), This);

      Close (F);
   end Load;

end Agpl.Generic_Limited_File_Store;
