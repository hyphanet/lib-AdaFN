 

package Agpl.Optimization.Mtsp is

   pragma Preelaborate;

   No_Solution : exception;

   type Cost_Matrix is array (Positive range <>,
                              Positive range <>,
                              Positive range <>) of Float;
   --  First index is the salesman index.
   --  Second and third is city index.
   --  Value should be float'last if not allowed, 0 for the same city.

   type Result_Matrix is array (Positive range <>,
                                Positive range <>) of Natural;
   --  First index is the salesman index.
   --  Second index is the city index.
   --  The value says in which stage the city is to be visited by the salesman.
   --  It will be zero for non-visited cities.

   type Start_Matrix is array (Positive range <>) of Positive;
   --  Index is the salesman index.
   --  Value is in which city it starts.

   type Stage_Matrix is array (Positive range <>) of Natural;
   --  Index is salesman.
   --  Value is how many cities has already visited.

   subtype Pos_Matrix is Start_Matrix;
   --  Index is salesman.
   --  Value is last visited city.

   function Brute_Force (Costs : in Cost_Matrix) return Result_Matrix;
   --  Tries all combinations and return the best assignation.
   --  All salesmen start at city 1.
   --  May raise No_Solution.

   function Get_Total_Cost (Costs : in Cost_Matrix;
                            Sol   : in Result_Matrix) return Float;
   --  Total cost incurred by all salesmen.

   function Get_Max_Min_Cost (Costs : in Cost_Matrix;
                              Sol   : in Result_Matrix) return Float;
   --  Cost of the worse salesman.

end Agpl.Optimization.Mtsp;
