 

with Agpl.Ip;
with Agpl.Strings.Fields;

with Ada.Text_io;

package body Agpl.Geoip is

   ----------------
   -- Parse_line --
   ----------------
   procedure Parse_line (Line : in String) is
      subtype LLInteger is Long_Long_Integer;
      Lbound : Long_Long_Integer;
      NEntry : Ip_entry;
      Name   : Ustring;
      use Agpl.Strings.Fields;
      use IP_maps;
      use Code_maps;
   begin
      LBound             := LLInteger'Value (Select_field (Line, 6, '"'));
      NEntry.Upper_bound := LLInteger'Value (Select_field (Line, 8, '"'));
      NEntry.Code        := Select_field (Line, 10, '"');
      Name               := U (Select_field (Line, 12, '"'));

      Insert (IP_table,   LBound, NEntry);
      Insert (Code_table, NEntry.Code, Name);
   end Parse_line;

   ------------------------------------------------------------------------
   -- Init                                                               --
   ------------------------------------------------------------------------
   --  Path to database
   procedure Init (Database : in String) is
      use Ada.Text_io;
      F    : File_type;
      Line : String (1 .. 255);
      Last : Natural;
   begin
      Open (F, Name => Database, Mode => In_file);
      while not End_of_file (F) loop
         Get_line (F, Line, Last);
         Parse_line (Line (Line'First .. Last));
      end loop;
      Close (F);
   exception
      when others =>
         if Is_open (F) then
            Close (F);
         end if;
         raise;
   end Init;

   ------------------------------------------------------------------------
   -- Country_code_by_addr                                               --
   ------------------------------------------------------------------------
   --  Addr in dot format
   function Country_code_from_addr (Addr : in String) return Country_code is
      use Ip_maps;
      N : constant Long_Long_Integer := Ip.To_number (Ip.Strip_port (Addr));
      I : Cursor := Floor (Ip_table, N);
   begin
      if Has_Element (I) then
         if Key (I) = N then
            return Element (I).Code; -- <-------- EXACT MATCH
         else
            Previous (I);
         end if;
      else
         Previous (I);
      end if;

      if I = No_Element then
         return Unknown_country;
      elsif Element (I).Upper_bound < N then
         return Unknown_country;
      else
         return Element (I).Code;
      end if;
   end Country_code_from_addr;

   ------------------------------------------------------------------------
   -- Country_name_from_addr                                             --
   ------------------------------------------------------------------------
   --  Addr in dot format, port optional
   function Country_name_from_addr (Addr : in String) return String is
   begin
      return Country_name_from_code (Country_code_from_addr (Addr));
   end Country_name_from_addr;

   ------------------------------------------------------------------------
   -- Country_name_from_code                                             --
   ------------------------------------------------------------------------
   --  Will work only for countries with existing IP ranges.
   --  "Unknown" otherwise.
   function Country_name_from_code (Code : in Country_code) return String is
      use Code_maps;
      I : constant Cursor := Find (Code_table, Code);
   begin
      if Has_Element (I) then
         return S (Element (I));
      else
         return "Unknown";
      end if;
   end Country_name_from_code;

   ------------------------------------------------------------------------
   -- Flag_code_from_country_code                                        --
   ------------------------------------------------------------------------
   --  returns "unknown" instead of "??", else the code
   function Flag_code_from_country_code (Code : in Country_code)
      return String
   is
   begin
      if Code = "??" then
         return "unknown";
      else
         return Code;
      end if;
   end Flag_code_from_country_code;

end Agpl.Geoip;
