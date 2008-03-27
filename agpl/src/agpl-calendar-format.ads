 

with Ada.Calendar;

package Agpl.Calendar.Format is

   pragma Elaborate_Body;

   --  Returns a beautified duration in hours, minutes, seconds, ms
   function Image (D : in Duration) return String;

   --  Returns as a string hh:mm:ss
   function Hour (T : in Ada.Calendar.Time) return String;

   --  Returns the date as yyyy/mm/dd
   function Datestamp (H         : in Ada.Calendar.Time := Ada.Calendar.Clock;
                       Separator : in Character := '/')
      return String;

   --  Returns the time as hh:mm:ss:dd
   function Timestamp (h : Ada.Calendar.Time := Ada.Calendar.Clock)
      return String;

end Agpl.Calendar.Format;
