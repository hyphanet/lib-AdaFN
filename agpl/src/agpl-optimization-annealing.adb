 

with Agpl.Chronos;
with Agpl.Trace; use Agpl.Trace;

with Ada.Numerics.Elementary_Functions;

package body Agpl.Optimization.Annealing is

   --------------------
   -- Lineal_Cooling --
   --------------------

   function Lineal_Cooling (T : in Temperature) return Temperature is
   begin
      return T - Temperature (1.0 / Float (Steps));
   exception
      when Constraint_Error =>
         if Cyclic then
            return Temperature'Last;
         else
            return Temperature'First; -- Exceeded iterations.
         end if;
   end Lineal_Cooling;

   --------------------------
   -- Proportional_Cooling --
   --------------------------

   function Proportional_Cooling (T : in Temperature) return Temperature is
   begin
      if Cyclic and then T < Umbral then
         return Temperature'Last;
      end if;

      return T * Temperature (Factor);
   end Proportional_Cooling;

   --------------------
   -- Cyclic_Cooling --
   --------------------

   function Cyclic_Cooling (T : in Temperature) return Temperature is
      pragma Unreferenced (T);
      use Ada.Calendar;
      use Ada.Numerics.Elementary_Functions;

      Elapsed : constant Float := Float ((Clock - Start) / Period);
      Remaind : constant Float := 1.0 - (Elapsed - Float'Floor (Elapsed));
   begin
      return Temperature (Remaind ** Power);
   end Cyclic_Cooling;
   --  T := ((Clock - Start) / Period) ^ Power
   --  Note that Start is reset if Clock - Start > Period

   --------------------
   -- Manual_Cooling --
   --------------------

   package body Manual_Cooling is

      Local_T : Temperature := Initial_Temperature;

      ---------------------
      -- Get_Temperature --
      ---------------------

      function Get_Temperature (T : in Temperature) return Temperature is
         pragma Unreferenced (T);
      begin
         return Local_T;
      end Get_Temperature;

      -----------
      -- Reset --
      -----------

      procedure Reset (Top : in Temperature := 1.0) is
      begin
         Local_T := Top;
      end Reset;

      ------------
      -- Divide --
      ------------

      procedure Divide (Denom : in Float := 2.0) is
      begin
         Local_T := Temperature (Float (Local_T) / Denom);
      end Divide;

      ------------
      -- Update --
      ------------

      Cool_Timer   : Chronos.Object;
      Settle_Timer : Chronos.Object;
      Prev_C       : Cost        := Cost'Last;
      Local_Min    : Cost        := Cost'Last;
      use Chronos;

      procedure Update (Current_Cost : in Cost) is
      begin
         if Current_Cost < Prev_C then
            Settle_Timer.Reset;
            Log ("Reseting Settle " & Image (Current_Cost) & " " &
                 Image (Prev_C), Debug, Detail_Section);
         end if;
         if Current_Cost < Local_Min then
            Local_Min := Current_Cost;
            Cool_Timer.Reset;
            Log ("Reseting Cooling " & Image (Current_Cost) & " " &
                 Image (Local_Min), Debug, Detail_Section);
         end if;

         if Elapsed (Cool_Timer) > Cool_Time then
            Divide (Divisor);
            Cool_Timer.Reset;
            Log ("Cooling...", Debug, Detail_Section);
         end if;

         if -- Local_T <= Settle_Umbral and then
           Elapsed (Settle_Timer) > Settle_Time
         then
            Local_T   := Ceiling_Temperature;
            Local_Min := Cost'Last;
            Settle_Timer.Reset;
            Cool_Timer.Reset;
            Log ("Temperature bump!", Debug, Detail_Section);
         end if;

         Prev_C := Current_Cost;
      end Update;

   end Manual_Cooling;

end Agpl.Optimization.Annealing;
