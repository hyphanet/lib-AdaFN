 

--  Type for generating a simple graph
--  Data values are floats, vertical scale is automatic,
--  horizontal is zoomable, always starting from the end.
--  multiple series are allowed.

with Agpl.Bmp.Draw;
with Agpl.Exceptions;

package body Agpl.Graph is

   ------------------------------------------------------------------------
   -- Add_sample                                                         --
   ------------------------------------------------------------------------
   procedure Add_sample (
      This : in out Object; Serie : in Natural; Sample : in Float)
   is
      use Lists;
   begin
      Append (This.Data (Serie), Sample);
      while Natural (Length (This.Data (Serie))) > This.Samples loop
         Delete_first (This.Data (Serie));
      end loop;
   end Add_sample;

   ------------------------------------------------------------------------
   -- Get_bmp                                                            --
   ------------------------------------------------------------------------
   function Get_bmp (This : in Object; Height : in Positive) return Bmp.Object
   is
      B        : Bmp.Object;
      Max, Min : Float;
      Gamut    : Float;
      use Lists;
      I : Cursor;
      X, Y, PrevX, PrevY : Integer;
   begin
      Bmp.Create (B, Width => This.Samples, Height => Height);
      Bmp.Draw.Delete (B, This.Bgcolor);
      Bmp.Set_checking (B, False);

      Get_max_min (This, Max => Max, Min => Min);
      Draw_axis (This, Max => Max, Min => Min, Height => Height, Canvas => B);

      if Max < Min then
         return B; -- No data, return empty graph
      end if;

      Gamut := Max - Min;
      if Gamut = 0.0 then
         Gamut := 1.0;
      end if;

      for N in This.Data'Range loop
         I := Last (This.Data (N));
         X := This.Samples;
         while Has_Element (I) loop
            if Max = Min then
               Y := Height;
            else
               Y := Integer (Float'Floor (
                  (Max - Element (I)) * Float (Height - 1) / Gamut) + 1.0);
            end if;
            if X = This.Samples then
               PrevY := Y;
               PrevX := X;
            end if;
            Bmp.Draw.Line (
               B,
               C1    => X,
               R1    => Y,
               C2    => PrevX,
               R2    => PrevY,
               Color => This.Fgcolor (N));
            PrevX := X;
            PrevY := Y;
            Previous (I);
            X := X - 1;
         end loop;
      end loop;

      return B;
   end Get_bmp;

   -----------------
   -- Get_max_min --
   -----------------
   --  Says max and min values in a graph
   procedure Get_max_min (This : in Object; Max, Min : out Float) is
      use Lists;
      I : Cursor;
   begin
      if This.Scale_min_forced then
         Min := This.Scale_min;
      else
         Min := Float'Last;
      end if;
      if This.Scale_max_forced then
         Max := This.Scale_max;
      else
         Max := Float'First;
      end if;

      --  Early exit if both ranges forced:
      if This.Scale_min_forced and then This.Scale_max_forced then
         return;
      end if;

      for N in This.Data'Range loop
         I := First (This.Data (N));
         while Has_Element (I) loop
            if not This.Scale_max_forced then
               Max := Float'Max (Max, Element (I));
            end if;
            if not This.Scale_min_forced then
               Min := Float'Min (Min, Element (I));
            end if;
            Next (I);
         end loop;
      end loop;
   end Get_max_min;

   ---------------
   -- Draw_axis --
   ---------------
   procedure Draw_axis (
      This     : in     Object;
      Max, Min : in     Float;
      Height   : in     Positive;
      Canvas   : in out Bmp.Object)
   is
      use Axis_vector;
      ----------------
      -- Draw_XAxis --
      ----------------
      procedure Draw_XAxis (Ax : in Axis_type) is
         pragma Unreferenced (Ax);
      begin
         raise Exceptions.Unimplemented;
      end Draw_XAxis;
      ----------------
      -- Draw_YAxis --
      ----------------
      procedure Draw_YAxis (Ax : in Axis_type) is
         Y  : Positive;
         FY : Float;
      begin
         if Min < Ax.Height and then Max > Ax.Height then
            Y := Positive (Float'Floor (
               (Max - Ax.Height) * Float (Height - 1) / (Max - Min)) + 1.0);
            Agpl.Bmp.Draw.Line (
               Canvas, R1 => Y, C1 => 1, R2 => Y, C2 => This.Samples,
               Color => Ax.Color);
         end if;
         if Ax.Repeat then
            --  Upwards
            FY := Ax.Height * 2.0;
            while FY < Max loop
               Y := Positive (Float'Floor (
                 (Max - FY) * Float (Height - 1) / (Max - Min)) + 1.0);
               Agpl.Bmp.Draw.Line (
                  Canvas, R1 => Y, C1 => 1, R2 => Y, C2 => This.Samples,
                  Color => Ax.Color);
               FY := FY + Ax.Height;
            end loop;
            --  Downwards
            FY := 0.0;
            while FY > Min loop
               Y := Positive (Float'Floor (
                 (Max - FY) * Float (Height - 1) / (Max - Min)) + 1.0);
               Agpl.Bmp.Draw.Line (
                  Canvas, R1 => Y, C1 => 1, R2 => Y, C2 => This.Samples,
                  Color => Ax.Color);
               FY := FY - Ax.Height;
            end loop;
         end if;
      end Draw_YAxis;
   begin
      for N in 1 .. Last (This.Axis) loop
         case This.Axis.Vector (N).Orientation is
            when Vertical   => Draw_XAxis (This.Axis.Vector (N));
            when Horizontal => Draw_YAxis (This.Axis.Vector (N));
         end case;
      end loop;
   end Draw_axis;

   ------------------------------------------------------------------------
   -- Set_colors                                                         --
   ------------------------------------------------------------------------
   procedure Set_colors (
      This    : in out Object;
      Bgcolor : in     Types.Rgb_triplet;
      Fgcolor : in     Types.Rgb_array) -- Must have Series elements.
   is
   begin
      This.Bgcolor := Bgcolor;
      This.Fgcolor := Fgcolor;
   end Set_colors;

   ------------------------------------------------------------------------
   -- Set_scale                                                          --
   ------------------------------------------------------------------------
   procedure Set_scale_min (This : in out Object; Min : in Float) is
   begin
      This.Scale_min_forced := True;
      This.Scale_min        := Min;
   end Set_scale_min;

   procedure Set_scale_max (This : in out Object; Max : in Float) is
   begin
      This.Scale_max_forced := True;
      This.Scale_max        := Max;
   end Set_scale_max;

   procedure Set_scale_auto (This : in out Object) is
   begin
      This.Scale_min_forced := False;
      This.Scale_max_forced := False;
   end Set_scale_auto;

   ------------------------------------------------------------------------
   -- Set_YAxis                                                          --
   ------------------------------------------------------------------------
   --  Repeat indicates if the axis will repeat x2, x3, etc.
   procedure Set_YAxis (
      This   : in out Object;
      Height : in     Float;
      Color  : in     Types.Rgb_triplet;
      Repeat : in     Boolean := False)
   is
      Axis : Axis_type (Horizontal);
   begin
      Axis.Repeat := Repeat;
      Axis.Color  := Color;
      Axis.Height := Height;
      Axis_vector.Append (This.Axis, Axis);
   end Set_YAxis;

   ------------------------------------------------------------------------
   -- Reset                                                              --
   ------------------------------------------------------------------------
   --  Removes all samples
   procedure Reset (This : in out Object) is
   begin
      for N in 1 .. This.Series loop
         Lists.Clear (This.Data (N));
      end loop;
   end Reset;

end Agpl.Graph;
