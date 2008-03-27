 

with Ada.Calendar;

--  OO implementation of the simulated annealing method

package Agpl.Optimization.Annealing is

   --  pragma Elaborate_Body;

   Log_Section    : constant String := "agpl.optimization.annealing";
   Detail_Section : constant String := "agpl.optimization.annealing.detail";

   type Temperature is new Float range 0.0 .. 1.0;
   --  Valid temperature ranges

   subtype Float_0_1 is Float range 0.0 .. 1.0;

   type Probability is new Float_0_1;

   subtype Acceptability is Probability;
   --  Probability of keeping a new solution

   generic
      Steps : Positive;
      Cyclic : Boolean := True;
   function Lineal_Cooling (T : in Temperature) return Temperature;
   --  (T := T - 1/Steps).
   --  If cyclic, Temp goes up to 1.0 if 0.0 is reached

   generic
      Factor : Float_0_1;
      Cyclic : Boolean     := True;
      Umbral : Temperature := 0.1; -- Point of reheating
   function Proportional_Cooling (T : in Temperature) return Temperature;
   --  (T := T * Factor)

   generic
      Start  : Ada.Calendar.Time;
      Period : Duration := 10.0;
      Power  : Float    := 1.0;
   function Cyclic_Cooling (T : in Temperature) return Temperature;
   --  T := ((Clock - Start) / Period) ^ Power
   --  Note that Start is reset if Clock - Start > Period

   generic
      Initial_Temperature : Temperature := 0.0;
      Ceiling_Temperature : Temperature := 0.002;
      --  When auto re-heating, go to this temperature

      Settle_Umbral       : Temperature := Temperature'Small * 2.0;
      --  When checking for no progress, this is the "absolute zero";
      --  if not reached, no check.

      Cool_Time           : Duration    := 0.5;
      --  Time without progress that will cause cooling.

      Settle_Time         : Duration    := 10.0;
      --  Time without progress under Settle_Umbral until re-heating

      Divisor             : Float       := 1.85;
      --  The amount to divide temperature if no progress
   package Manual_Cooling is
      --  Expected package usage is to manually divide temperature when you need
      --  it.
      --  In this way you can keep low temperatures for as long as necessary.
      --  Or, using Update, it will be done for you

      function Get_Temperature (T : in Temperature) return Temperature;
      pragma Inline (Get_Temperature);
      --  returns the same temperature

      procedure Reset (Top : in Temperature := 1.0);
      --  Resets temperature to 1.0

      procedure Divide (Denom : in Float := 2.0);
      --  Divide temperature by the given Denominator

      procedure Update (Current_Cost : in Cost);
      --  To use auto updating
   end Manual_Cooling;

end Agpl.Optimization.Annealing;
