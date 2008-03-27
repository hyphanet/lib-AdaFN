 

--  Utilities for IP conversions

with Agpl.Strings.Fields;

package body Agpl.Ip is

   ------------------------------------------------------------------------
   -- Strip_port                                                         --
   ------------------------------------------------------------------------
   --  Removes the port portion, if present
   function Strip_port (Address : in Any_address) return Any_address is
      use Agpl.Strings.Fields;
   begin
      return Select_field (Address, 1, ':');
   end Strip_port;

   ------------------------------------------------------------------------
   -- To_number                                                          --
   ------------------------------------------------------------------------
   --  Returns a dotted_address as long long integer
   --  w.x.y.z = w * 256**3 + x * 256**2 + y * 256**1 + z
   function To_number (Address : in Dotted_address) return Long_Long_Integer
   is
      Result : Long_Long_Integer := 0;
      use Agpl.Strings.Fields;
   begin
      for N in 1 .. 4 loop
         Result := Result + Long_Long_Integer'Value (
            Select_field (Address, N, '.')) * 256**(4 - N);
      end loop;

      return Result;
   end To_number;

end Agpl.Ip;
