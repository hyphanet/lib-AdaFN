 

--  Packages for work with BMP files

with Agpl.Streams.Memory_arrays;

with Ada.Unchecked_deallocation;

package body Agpl.Bmp is

   package ADS renames Ada.Streams;
   package AGS renames Agpl.Streams;

   use type ADS.Stream_element_array;
   use type ADS.Stream_element_offset;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   procedure Create (
      This : in out Object; Width : in Positive; Height : in Positive)
   is
   begin
      Finalize (This);
      This.Data   :=
         new Ada.Streams.Stream_element_array (
            1 .. Ada.Streams.Stream_element_offset (Width * Height * 3));
      This.Width  := Width;
      This.Height := Height;
   end Create;

   ------------------------------------------------------------------------
   -- Check_coordinates                                                  --
   ------------------------------------------------------------------------
   --  Return true if inside bounds, false if outside, exception if checking
   function Check_coordinates (This : in Object; Row, Column : in Integer)
      return Boolean;
   pragma Inline (Check_coordinates);
   function Check_coordinates (This : in Object; Row, Column : in Integer)
      return Boolean is
   begin
      if Row < 1 or else Column < 1 or else
         Row > This.Height or else Column > This.Width
      then
         if not This.Checking then
            return False;
         else
            raise Coordinates_out_of_bounds;
         end if;
      else
         return True;
      end if;
   end Check_coordinates;
   ------------------------------------------------------------------------
   -- Index_of                                                          --
   ------------------------------------------------------------------------
   function Index_of (This : in Object; Row, Column : in Positive)
      return Ada.Streams.Stream_element_offset;
   pragma Inline (Index_of);
   function Index_of (This : in Object; Row, Column : in Positive)
      return Ada.Streams.Stream_element_offset is
   begin
      return ADS.Stream_element_offset (
         (This.Height - Row) * This.Width * 3 + (Column - 1) * 3 + 1);
   end Index_of;

   ------------------------------------------------------------------------
   -- Get_pixel                                                          --
   ------------------------------------------------------------------------
   function Get_pixel (
      This   : in Object;
      Row,
      Column : in Integer) return Types.Rgb_triplet
   is
      Pos : ADS.Stream_element_offset;
   begin
      if not Check_coordinates (This, Row, Column) then
         raise Coordinates_out_of_bounds;
      end if;
      Pos := Index_of (This, Row, Column);
      return (
         B => Types.Unsigned_8 (This.Data (Pos)),
         G => Types.Unsigned_8 (This.Data (Pos + 1)),
         R => Types.Unsigned_8 (This.Data (Pos + 2)));
   end Get_pixel;

   ------------------------------------------------------------------------
   -- Set_pixel                                                          --
   ------------------------------------------------------------------------
   procedure Set_pixel (
      This   : in out Object;
      Row,
      Column : in     Integer;
      Rgb    : in     Types.Rgb_triplet)
   is
      Pos : ADS.Stream_element_offset;
   begin
      if not Check_coordinates (This, Row, Column) then
         return;
      end if;

      Pos := Index_of (This, Row, Column);
      This.Data (Pos)     := ADS.Stream_element (Rgb.B);
      This.Data (Pos + 1) := ADS.Stream_element (Rgb.G);
      This.Data (Pos + 2) := ADS.Stream_element (Rgb.R);
   end Set_pixel;

   ------------------------------------------------------------------------
   -- Set_checking                                                       --
   ------------------------------------------------------------------------
   --  If drawing outbounds, we can get an error or silent discarding:
   procedure Set_checking (This : in out Object; Check : in Boolean := True)
   is
   begin
      This.Checking := Check;
   end Set_checking;

   ------------------------------------------------------------------------
   -- Get_stream                                                         --
   ------------------------------------------------------------------------
   --  Returns a stream with a valid BMP representation (not the pixel matrix).
   function Get_stream (This : in Object)
      return Ada.Streams.Stream_element_array
   is
      Headers : aliased ADS.Stream_element_array := (1 .. 54 => 0);
      Stream  : aliased AGS.Memory_arrays.Stream_type (Headers'Access);
      Str     : constant AGS.Stream_access := Stream'Unchecked_Access;

      use Interfaces;
   begin
      --  HEADER
      --  Magic
      String'Write (Str, "BM");
      --  File size
      Unsigned_32'Write (Str, Unsigned_32(Headers'Length + This.Data'Length));
      --  Reserved
      Unsigned_32'Write (Str, 0);
      --  Offset of image data
      Unsigned_32'Write (Str, Unsigned_32 (Headers'Length));

      --  INFOHEADER
      --  Size of infoheader?
      Unsigned_32'Write (
         Str,
         Unsigned_32 (Headers'Length - AGS.Memory_arrays.Index (Stream)));
      --  Width
      Unsigned_32'Write (Str, Unsigned_32 (This.Width));
      --  Height
      Unsigned_32'Write (Str, Unsigned_32 (This.Height));
      --  Color planes
      Unsigned_16'Write (Str, 1);
      --  bpp
      Unsigned_16'Write (Str, 24);
      --  No compression
      Unsigned_32'Write (Str, 0);
      --  Data size
      Unsigned_32'Write (Str, Unsigned_32 (This.Data'Length));
      --  Pixels per meter, horizontal and vertical
      Unsigned_32'Write (Str, 0);
      Unsigned_32'Write (Str, 0);
      --  Palette colors
      Unsigned_32'Write (Str, 0);
      --  Important colors
      Unsigned_32'Write (Str, 0);

      if AGS.Memory_arrays.Index (Stream) /= Headers'Last then
         raise Constraint_Error;
      end if;

      return Headers & This.Data.all;
   end Get_stream;

   procedure Free is new Ada.Unchecked_deallocation (
      Ada.Streams.Stream_element_array,
      Agpl.Streams.Stream_element_array_access);

   procedure Initialize (This : in out Object) is
      pragma Unreferenced (This);
   begin
      null;
   end Initialize;

   procedure Adjust     (This : in out Object) is
   begin
      This.Data := new ADS.Stream_element_array'(This.Data.all);
   end Adjust;

   procedure Finalize   (This : in out Object) is
   begin
      Free (This.Data);
   end Finalize;

end Agpl.Bmp;
