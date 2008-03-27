 

with Agpl.Strings.Fields;

with Ada.Streams.Stream_Io;

package body Agpl.Filesystem is

   ------------------
   -- Ensure_Slash --
   ------------------

   function Ensure_Slash (This : in String; Separator : in Character := '/')
      return String is
   begin
      if This (This'Last) /= Separator then
         return This & Separator;
      else
         return This;
      end if;
   end Ensure_Slash;

   -----------------------
   -- Replace_Extension --
   -----------------------

   function Replace_Extension (This : in String; New_Ext : in String)
                               return    String
   is
   begin
      return Strings.Fields.String_Tail_Reverse (This, '.') & '.' & New_Ext;
   end Replace_Extension;

   ---------------
   -- Read_File --
   ---------------

   function Read_File (Name : String) return Ustring is
      Result : Ustring;
      use Ada.Streams.Stream_Io;
      F      : File_Type;
   begin
      Open (F, In_File, Name);
      declare
         Length : constant Natural := Natural (Size (F));
         Remain :          Natural := Length;
         Stream : constant Stream_Access := Ada.Streams.Stream_Io.Stream (F);
      begin
         while Remain > 0 loop
            declare
               Chunk : String (1 .. Natural'Min (Remain, 1000));
            begin
               String'Read (Stream, Chunk);
               Asu.Append (Result, Chunk);
               Remain := Remain - Chunk'Length;
            end;
         end loop;
      end;
      Close (F);
      return Result;
   exception
      when others =>
         Close (F);
         raise;
   end Read_File;

end Agpl.Filesystem;
