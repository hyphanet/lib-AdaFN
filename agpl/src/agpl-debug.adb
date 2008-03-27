 

--  Facilities for debugging.

with Agpl.Conversions;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

package body Agpl.Debug is

   use type Ustring;

   ------------------------------------------------------------------------
   -- Report                                                             --
   ------------------------------------------------------------------------
   --  Constructs a error string upon exception:
   function Report (E : Exceptions.Exception_occurrence) return String is
   begin
      return Exceptions.Exception_information (E);
   end Report;

   ------------------------------------------------------------------------
   -- Hex_Dump_From_Stream                                               --
   ------------------------------------------------------------------------
   --  Returns next N characters from a stream as hex
   function Hex_Dump_From_Stream (
      Stream      : access Streams.Root_stream_type'Class;
      N           : in     Positive := 8;
      Separator   : in     String   := ":") return String
   is
      S : String (1 .. N);
      R : UString;
   begin
      String'Read (Stream, S);
      for N in S'Range loop
         R := R & "0x" & Conversions.To_Hex (S (N));
         if N /= S'Last then
            R := R & Separator;
         end if;
      end loop;

      return To_String (R);
   end Hex_Dump_From_Stream;

end Agpl.Debug;
