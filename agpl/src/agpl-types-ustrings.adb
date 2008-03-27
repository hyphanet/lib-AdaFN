 

package body Agpl.Types.Ustrings is

   ------------------------------------------------------------------------
   -- Write_To_Stream                                                    --
   ------------------------------------------------------------------------
   --  Writes the Ustring contents to a stream
   procedure Write_To_Stream (This : in Ustring; Stream : access Ada.Streams.Root_Stream_Type'Class) is
   begin
      for I in 1 .. ASU.Length (This) loop
         Character'Write (Stream, ASU.Element (This, I));
      end loop;
   end Write_To_Stream;

end Agpl.Types.Ustrings;
