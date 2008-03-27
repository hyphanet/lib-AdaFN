 

--  Type for generating a simple graph
--  Data values are floats, vertical scale is automatic, horizontal is fixed,
--  multiple series are allowed. Values in multiple series should be normali-
--  zed against a common scale.

with Agpl.Bmp;
with Agpl.Constants;
with Agpl.Dynamic_vector;
with Agpl.Types;

with Ada.Containers.Doubly_Linked_Lists;

package Agpl.Graph is

   --  pragma Elaborate_Body;

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   type Object (Series : Positive; Samples : Positive) is tagged limited private;

   ------------------------------------------------------------------------
   -- Add_sample                                                         --
   ------------------------------------------------------------------------
   procedure Add_sample (
      This : in out Object; Serie : in Natural; Sample : in Float);

   ------------------------------------------------------------------------
   -- Get_bmp                                                            --
   ------------------------------------------------------------------------
   function Get_bmp (This : in Object; Height : in Positive)
      return Bmp.Object;

   ------------------------------------------------------------------------
   -- Reset                                                              --
   ------------------------------------------------------------------------
   --  Removes all samples
   procedure Reset (This : in out Object);

   ------------------------------------------------------------------------
   -- Set_colors                                                         --
   ------------------------------------------------------------------------
   procedure Set_colors (
      This    : in out Object;
      Bgcolor : in     Types.Rgb_triplet;
      Fgcolor : in     Types.Rgb_array); -- Must have Series elements.

   ------------------------------------------------------------------------
   -- Set_scale                                                          --
   ------------------------------------------------------------------------
   procedure Set_scale_min (This : in out Object; Min : in Float);
   procedure Set_scale_max (This : in out Object; Max : in Float);
   procedure Set_scale_auto (This : in out Object);

   ------------------------------------------------------------------------
   -- Set_YAxis                                                          --
   ------------------------------------------------------------------------
   --  Repeat indicates if the axis will repeat x2, x3, etc.
   procedure Set_YAxis (
      This   : in out Object;
      Height : in     Float;
      Color  : in     Types.Rgb_triplet;
      Repeat : in     Boolean := False);

private

   package Lists is new Ada.Containers.Doubly_Linked_Lists (
      Float, "=");

   type List_array is array (Positive range <>) of Lists.List;

   type Orientations is (Horizontal, Vertical);

   type Axis_type (Orientation : Orientations := Horizontal) is record
      Color  : Types.Rgb_triplet;
      Repeat : Boolean;
      case Orientation is
         when Horizontal =>
            Height : Float;
         when Vertical =>
            Width : Positive;
      end case;
   end record;

   package Axis_vector is new Agpl.Dynamic_vector (Axis_type);

   type Object (Series : Positive; Samples : Positive) is tagged limited record
      Data    : List_array (1 .. Series);
      Bgcolor : Types.Rgb_triplet := Constants.Black;
      Fgcolor : Types.Rgb_array (1 .. Series) := (others => Constants.White);

      Scale_min_forced : Boolean := False;
      Scale_max_forced : Boolean := False;
      Scale_min        : Float;
      Scale_max        : Float;

      Axis    : Axis_vector.Object (First => 1);
   end record;

   -----------------
   -- Get_max_min --
   -----------------
   --  Says max and min values in a graph
   --  Uses forced extremes if present
   procedure Get_max_min (This : in Object; Max, Min : out Float);

   ---------------
   -- Draw_axis --
   ---------------
   procedure Draw_axis (
      This     : in     Object;
      Max, Min : in     Float;
      Height   : in     Positive;
      Canvas   : in out Bmp.Object);

end Agpl.Graph;
