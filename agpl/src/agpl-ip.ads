 

--  Utilities for IP conversions

package Agpl.Ip is

   pragma Preelaborate;

   --  Named or dotted address:
   subtype Any_address is String;

   --  x.x.x.x[:port]
   subtype Dotted_address is String;

   --  named address
   subtype Named_address is String;

   ------------------------------------------------------------------------
   -- Strip_port                                                         --
   ------------------------------------------------------------------------
   --  Removes the port portion, if present
   function Strip_port (Address : in Any_address) return Any_address;

   ------------------------------------------------------------------------
   -- To_number                                                          --
   ------------------------------------------------------------------------
   --  Returns a dotted_address as long long integer
   --  w.x.y.z = w * 256**3 + x * 256**2 + y * 256**1 + z
   function To_number (Address : in Dotted_address) return Long_Long_Integer;

end Agpl.Ip;
