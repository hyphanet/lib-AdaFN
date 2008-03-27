 

--  Packages for work with BMP files

with Agpl.Streams;
with Agpl.Types;

with Interfaces;

with Ada.Finalization;
with Ada.Streams;

package Agpl.Bmp is

   pragma Preelaborate;

   --  Mime_type
   Mime_type : constant String := "image/bmp";

   --  Exception if drawing out of bounds:
   Coordinates_out_of_bounds   : exception;

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   --  A Bmp object. Only 24bpp, uncompressed are valid ATM.
   type Object is tagged private;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   procedure Create (
      This : in out Object; Width : in Positive; Height : in Positive);

   ------------------------------------------------------------------------
   -- Get_pixel                                                          --
   ------------------------------------------------------------------------
   function Get_pixel (
      This   : in Object;
      Row,
      Column : in Integer) return Types.Rgb_triplet;

   ------------------------------------------------------------------------
   -- Set_pixel                                                          --
   ------------------------------------------------------------------------
   procedure Set_pixel (
      This   : in out Object;
      Row,
      Column : in     Integer;
      Rgb    : in     Types.Rgb_triplet);

   ------------------------------------------------------------------------
   -- Set_checking                                                       --
   ------------------------------------------------------------------------
   --  If drawing outbounds, we can get an error or silent discarding:
   procedure Set_checking (This : in out Object; Check : in Boolean := True);

   ------------------------------------------------------------------------
   -- Get_stream                                                         --
   ------------------------------------------------------------------------
   --  Returns a stream with a valid BMP representation (not the pixel matrix).
   function Get_stream (This : in Object)
      return Ada.Streams.Stream_element_array;

private

   pragma Inline (Get_pixel, Set_pixel);

   --  Data types
   type Short_int is new Interfaces.Integer_16;
   type Int       is new Interfaces.Integer_32;

   type Object is new Ada.Finalization.Controlled with record
      --  Pixels
      Data   : Agpl.Streams.Stream_element_array_access;
      --  Dimensions
      Width  : Positive;
      Height : Positive;
      --  Checking
      Checking : Boolean := True;
   end record;

   procedure Initialize (This : in out Object);
   procedure Adjust     (This : in out Object);
   procedure Finalize   (This : in out Object);

end Agpl.Bmp;
