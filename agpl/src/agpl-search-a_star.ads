--  Standard A* search
generic
   type State is private;
   type Costs is private;
   Zero : Costs;
   with function "<" (L, R : Costs) return Boolean is <>;
   with function "+" (L, R : Costs) return Costs is <>;
   with function Image (L  : Costs) return String;
package Agpl.Search.A_Star is

   --  pragma Preelaborate;

   Log_Section : constant String := "agpl.search.a*";

   generic
      with function Num_Next (S : State) return Natural;
      with function Next (S : State; I : Positive) return State;
      --  Enumeration of neighbors
      with function Real_Cost (Ini,
                               Fin : State) return Costs;
      --  True cost between neighbors
      with function Estimate (Ini,
                              Fin : State) return Costs;
      --  Optimistic heuristic estimator
      with function Image (S   : State) return String;
      --  Must be unique for each state
      type Path is private;
      with procedure Prepend (R : in out Path; S : State);
      --  Reversed building of result
      procedure Best_Path (Ini,
                           Fin   :     State;
                           Route : out Path;
                           Cost  : out Costs);

end Agpl.Search.A_Star;
