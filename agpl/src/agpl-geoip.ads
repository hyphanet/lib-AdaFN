 

--  Uses the CSV database from www.maxmind.com

with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Ada.Containers.Ordered_Maps;

package Agpl.Geoip is

   --  pragma Elaborate_Body;

   ------------------------------------------------------------------------
   -- Types                                                              --
   ------------------------------------------------------------------------
   subtype Country_code is String (1 .. 2);

   Unknown_country : constant Country_code := "??";

   ------------------------------------------------------------------------
   -- Init                                                               --
   ------------------------------------------------------------------------
   --  Call it before any other subroutine.
   --  Path to database.
   procedure Init (Database : in String);

   ------------------------------------------------------------------------
   -- Country_code_from_addr                                             --
   ------------------------------------------------------------------------
   --  Addr in dot format, port optional
   function Country_code_from_addr (Addr : in String) return Country_code;

   ------------------------------------------------------------------------
   -- Country_name_from_addr                                             --
   ------------------------------------------------------------------------
   --  Addr in dot format, port optional
   function Country_name_from_addr (Addr : in String) return String;

   ------------------------------------------------------------------------
   -- Country_name_from_code                                             --
   ------------------------------------------------------------------------
   --  Will work only for countries with existing IP ranges.
   --  "Unknown" otherwise.
   function Country_name_from_code (Code : in Country_code) return String;

   ------------------------------------------------------------------------
   -- Flag_code_from_country_code                                        --
   ------------------------------------------------------------------------
   --  returns "unknown" instead of "??", else the code
   function Flag_code_from_country_code (Code : in Country_code)
      return String;
   pragma Inline (Flag_code_from_country_code);

private

   type Ip_entry is record
      Upper_bound : Long_Long_Integer; -- IP
      Code        : String (1 .. 2);   -- Country code
   end record;

   package IP_maps is new Ada.Containers.Ordered_Maps (
      Long_Long_Integer, Ip_entry, "<", "=");

   IP_table : IP_maps.Map;

   package Code_maps is new Ada.Containers.Ordered_Maps (
      Country_code, Ustring, "<", Agpl.Types.Ustrings.ASU."=");

   Code_table : Code_maps.Map;

   --  Parses a line of CSV and adds it to the tables
   procedure Parse_line (Line : in String);

end Agpl.Geoip;
