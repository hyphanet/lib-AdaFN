 

with Agpl.Strings;
with Agpl.Trace; use Agpl.Trace;

with Ada.Strings;
with Ada.Strings.Fixed;

with GNAT.Calendar;

with Text_IO;

package body Agpl.Calendar.Format is

   -----------------------------------
   -- Image                         --
   -----------------------------------
   --  Returns a beautified duration in hours, minutes, seconds, milliseconds
   function Image (D : Duration) return String is
      S  : String (1 .. 17) :=
        (3      => 'h',
         7      => 'm',
         11     => 's',
         16     => 'm',
         17     => 's',
         others => ' ');
      S2 : String (1 .. 10);
      package Int_io is new Text_IO.Integer_IO (Integer);
      use Int_io;
      Seconds : constant Integer := Integer (Float'Floor (Float (D)));
      use Ada.Strings;
      use Ada.Strings.Fixed;
   begin
      Put (S (5 .. 6), (Seconds rem 3600) / 60);
      Put (S (9 .. 10), Seconds rem 60); -- This is the line causing exceptions.
      Put (S (13 .. 15),
           Integer (1000.0 * Duration'(D - Duration (Seconds))));
      if Seconds / 3600 > 99 then
         Put (S2, Seconds / 3600);
         return Trim (S2, Both) & "h " & S (5 .. S'Last);
      else
         Put (S (1 .. 2), Seconds / 3600);
         return S;
      end if;
   exception
      when E : others =>
         Log ("Agpl.Calendar.Format.Image: " & Report (E), Debug);
         return "??h ??m ??s ???ms";
   end Image;

   ------------------------------------------------------------------------
   -- Hour                                                               --
   ------------------------------------------------------------------------
   --  Returns as a string hh:mm:ss
   function Hour (T : in Ada.Calendar.Time) return String is
      use Agpl.Strings;
   begin
      return Rpad (To_String (GNAT.Calendar.Hour (T)), 2, '0') &
             ":" &
             Rpad (To_String (GNAT.Calendar.Minute (T)), 2, '0') &
             ":" &
             Rpad (To_String (GNAT.Calendar.Second (T)), 2, '0');
   end Hour;

   ------------------------------------------------------------------------
   -- Timestamp                                                          --
   ------------------------------------------------------------------------
   function Timestamp
     (h    : Ada.Calendar.Time := Ada.Calendar.Clock)
      return String
   is
      use Ada.Calendar;
      package Int_Io is new Text_IO.Integer_IO (Integer);
      use Int_Io;
      s   : String (1 .. 11) := (3 => ':', 6 => ':', 9 => '.', others => '0');
      d   : Day_Duration;
      seg : Integer;
   begin
      d   := Seconds (h);
      seg := Integer (d * 100);
      Put (s (1 .. 2), seg / (60 * 60 * 100));
      Put (s (4 .. 5), (seg rem 360000) / 6000);
      Put (s (7 .. 8), (seg rem 6000) / 100);
      Put (s (10 .. 11), seg rem 100);
      for i in  s'Range loop
         if s (i) = ' ' then
            s (i) := '0';
         end if;
      end loop;
      return s;
   exception
      when others =>
         return "??:??:??.??";
   end Timestamp;

   function Datestamp (H         : in Ada.Calendar.Time := Ada.Calendar.Clock;
                       Separator : in Character := '/')
                       return String
   is
      --  yyyy.mm.dd
      use Ada.Calendar;
      package Int_Io is new Text_IO.Integer_IO (Integer);
      use Int_Io;
      s   : String (1 .. 10) := (5 => Separator, 8 => Separator, others => <>);
   begin
      Put (s (1 .. 4), Year (H));
      Put (s (6 .. 7), Month (H));
      Put (s (9 .. 10), Day (H));
      for i in  s'Range loop
         if s (i) = ' ' then
            s (i) := '0';
         end if;
      end loop;
      return s;
   exception
      when others =>
         return "????" & Separator & "??" & Separator & "?";
   end Datestamp;

end Agpl.Calendar.Format;
