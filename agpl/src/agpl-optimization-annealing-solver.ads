 

with Agpl.Generic_Handle;

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;

--  OO implementation of the simulated annealing method

generic
   type Solution (<>) is private;
   --  An opaque type containing a solution.

   with function Evaluate (Sol : in Solution) return Cost;
   --  Says how good is a solution.

   with procedure Mutate (Sol : in out Solution);
   --  Mutates a solution.

   with function Normalize (Old_Cost,
                            New_Cost : in Cost;
                            Temp     : in Temperature) return Acceptability;
   --  Say the probability of keeping a new solution, given the change in
   --  costs and current temperature.

   with function Last_Mutation (Sol : in Solution) return String;
   --  Informative, to know mutations working well
   --  Just returns a description of what was done.
   --  Should be unique for the mutation class, since it is used to
   --  aggregate stats.

   with procedure Undo (Sol : in out Solution);
   --  Must undo the last mutation. Only one level of undo is required.
package Agpl.Optimization.Annealing.Solver is

--   pragma Elaborate_Body;

   Log_Section : constant String := "agpl.optimization.annealing.solver";

   type Object is tagged limited private;
   --  The object used to perform the annealing

   function Best_Cost (This : in Object) return Cost;

   function Best_Solution (This : in Object) return Solution;
   --  Obtain the best solution seen till moment.

   function Current_Cost (This : in Object) return Cost;

   function Current_Solution (This : in Object) return Solution;

   function Current_Temperature (This : in Object) return Temperature;

   procedure Iterate (This   : in out Object;
                      Anneal : not null access function
                        (T : in Temperature) return Temperature);
   --  Do an iteration, and change the temperature. See parent package for
   --  some temperature change predefined functions.

   procedure Set_Initial_Solution (This : in out Object;
                                   Sol  : in     Solution);
   --  Starting solution

   procedure Set_Best_Solution (This : in out Object;
                                Sol  : in     Solution);
   --  If by some reason you alter it and need to replace...

   procedure Set_Current_Solution (This : in out Object;
                                   Sol  : in     Solution);
   --  Set the solution to be used as seed in the next iteration

   procedure Solve (This       : in out Object;
                    Ini_Sol    : in     Solution;
                    Anneal     : not null access function
                      (T : in Temperature) return Temperature;
                    Iterations : in     Positive;
                    Timeout    : in     Duration;
                    Converge   : in     Duration;
                    Progress   : access procedure
                      (Continue : out Boolean) := null);
   --  Run until Timeout expires or Converge time elapses without a better
   --  solution found or Iterations are performed.
   --  Callback is called once every second, just in case you want to do smthing

   procedure Work (This                     : in out Object;
                   Anneal                   : not null access function
                     (T : in Temperature) return Temperature;
                   Iterations               : in     Positive;
                   Timeout                  : in     Duration;
                   Converge                 : in     Duration;
                   Progress                 : access procedure
                     (Continue : out Boolean) := null;
                   Inform_At_End            : in     Boolean := False);
   --  As previous, but doesn't require an initial solution: assumes one exists
   --  and that everything is ready.
   --  This allows "chunking" the computation

   procedure Print_Stats (This : in Object);
   procedure Reset_Stats (This : in out Object);

private

   package Sol_Handle is new Generic_Handle (Solution);

   type Move_Stats is record
      Taken    : Natural := 0;
      Accepted : Natural := 0;
   end record;

   package Stat_Maps is
      new Ada.Containers.Indefinite_Ordered_Maps (String, Move_Stats);

   type Object is tagged limited record
      Best_Sol   : Sol_Handle.Object;
      Curr_Sol   : Sol_Handle.Object;

      Best_Cost  : Cost := Cost'Last;
      Curr_Cost  : Cost;

      Curr_Temp  : Temperature := Temperature'Last;

      Random_Gen : Generator; -- Here, to make things reproducible.

      Iterations : Natural := 0; -- Total iterations run
      Discarded  : Natural := 0; -- Discarded moves
      Wasted     : Natural := 0; -- Invalid mutations seen

      Stats      : Stat_Maps.Map;
   end record;

   procedure Add_Move (This     : in out Object;
                       Move     : in     String;
                       Accepted : in     Boolean);

   procedure Print_Stats (Stats : in Stat_Maps.Map);

end Agpl.Optimization.Annealing.Solver;
