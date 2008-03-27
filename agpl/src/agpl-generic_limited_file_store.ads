with Ada.Streams;

generic
   type Item (<>) is limited private;
   with procedure Write (S : access Ada.Streams.Root_Stream_Type'Class;
                         I :        Item);
   with procedure Read  (S : access Ada.Streams.Root_Stream_Type'Class;
                         I :    out Item);
package Agpl.Generic_Limited_File_Store is

   type Item_Access is access all Item;

   procedure To_File (This : in Item; File : in String);
   --  This uses Item'Write
   --  Write a file with the dumped item. Overwrites if existing.

   procedure Load (This : out Item; File : in String);
   --  This uses Item'Read
   --  In place to avoid extra copy at return.

end Agpl.Generic_Limited_File_Store;
