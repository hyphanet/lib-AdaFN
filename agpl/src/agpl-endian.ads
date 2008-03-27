 

package Agpl.Endian is

   pragma Elaborate_Body;

   --  Auxiliary types for later functions:
   type Byte is mod 2 ** 8;
   for Byte'Size use 8;

   type Byte_array is array (Integer range <>) of Byte;
   pragma Pack (Byte_array);

   --  Endingness of machine (determined at elaboration time):
   Little_Endian: Boolean;

   --  Convert an arbitrary long byte array in any endingness to integer
   --  May raise Constraint_error if array lengths exceedes integer capacity.
   function Convert (
      From        : Byte_array;
      Big_endian  : Boolean := True) return Integer;
   pragma Inline (Convert);

   function Convert_L (
      From        : Byte_array;
      Big_endian  : Boolean := True) return Long_Long_Integer;
   pragma Inline (Convert_L);

   --  Converts an integer to an array of bytes, in the desired endianness.
   --  Optimally returns the shortest possible array:
   --  I.e, 0 is returned as an empty array.
   function Convert (
      From        : Long_Long_Integer;
      Big_endian  : Boolean := False) return Byte_array;
   pragma Inline (Convert);

   --  Converts an integer to an array of bytes, in the desired endianness.
   --  Size specified (in bytes):
   function Convert (
      From        : Long_Long_Integer;
      Size        : Natural;
      Big_endian  : Boolean := False) return Byte_array;
   pragma Inline (Convert);

   --  Converts an integer to an array of bytes, in the desired endianness.
   --  Optimally returns the shortest possible array:
   --  I.e, 0 is returned as an empty array.
   function Convert (
      From        : Integer;
      Big_endian  : Boolean := False) return Byte_array;
   pragma Inline (Convert);

   --  Converts an integer to an array of bytes, in the desired endianness.
   --  Size specified (in bytes):
   function Convert (
      From        : Integer;
      Size        : Natural;
      Big_endian  : Boolean := False) return Byte_array;
   pragma Inline (Convert);

   --  Converts a byte array into a string of same bytes:
   function To_string (From : Byte_array) return String;

   --  Inverse of the previous:
   function To_byte_array (From : String) return Byte_array;

   --  Invert a byte_array order
   function Invert (From : Byte_array) return Byte_array;

private

   function Is_little_endian return Boolean;

end Agpl.Endian;
