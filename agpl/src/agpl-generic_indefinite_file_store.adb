with Ada.Streams.Stream_Io;

package body Agpl.Generic_Indefinite_File_Store is

   -------------
   -- To_File --
   -------------

   procedure To_File (This : in Item; File : in String) is
      use Ada.Streams.Stream_Io;
      F : File_Type;
   begin
      Create (F, Name => File, Mode => Out_File);

      Item'Output (Stream (F), This);
      Close (F);
   end To_File;

   ----------
   -- Load --
   ----------

   function Load (File : in String) return Item is
      use Ada.Streams.Stream_Io;
      F : File_Type;
   begin
      Open (F, Name => File, Mode => In_File);

      declare
         Result : constant Item := Item'Input (Stream (F));
      begin
         Close (F);
         return Result;
      end;
   end Load;

end Agpl.Generic_Indefinite_File_Store;
