 

--  Facilities for debugging.

with Ada.Exceptions;
with Ada.Streams;
use  Ada;

--  with Gnat.Debug_Pools;

package Agpl.Debug is

   pragma Preelaborate;

--   Pool : Gnat.Debug_Pools.Debug_Pool;

   ------------------------------------------------------------------------
   -- Report                                                             --
   ------------------------------------------------------------------------
   --  Constructs a error string upon exception:
   function Report (E : Exceptions.Exception_occurrence) return String;

   ------------------------------------------------------------------------
   -- Hex_Dump_From_Stream                                               --
   ------------------------------------------------------------------------
   --  Returns next N characters from a stream as hex
   function Hex_Dump_From_Stream (
      Stream      : access Streams.Root_stream_type'Class;
      N           : in     Positive := 8;
      Separator   : in     String   := ":") return String;

end Agpl.Debug;
