with Agpl.Trace;

use Agpl.Trace;

package body Agpl.Containers.Graphs.Algorithms is

   -------------------
   -- Compound_Cost --
   -------------------

   function Compound_Cost (V_New  : Vertex_Sets.Set;
                           V_Cost : Vertex_Edge_Type_Maps.Map;
                           Add    : Edge_Cursor'Class)
                           return   Edge_Type
   is
      function Compound (V_New  : Vertex_Cursor'Class;
                         V_Cost : Vertex_Edge_Type_Maps.Map;
                         Add    : Edge_Type)
                         return   Edge_Type
      is
          Prev : Edge_Type;
      begin
         if V_Cost.Contains (V_New) then
            Prev := V_Cost.Element (V_New);
         else
            Prev := Zero;
         end if;
         return Prev + Add;
      end Compound;
   begin
      if V_New.Contains (Add.First) then
         return Compound (Add.First, V_Cost, Add.Element);
      else
         return Compound (Add.Last, V_Cost, Add.Element);
      end if;
   end Compound_Cost;

   --------------
   -- Cheapest --
   --------------

   function Cheapest (V_New  : Vertex_Sets.Set;
                      V_Cost : Vertex_Edge_Type_Maps.Map;
                      Limit  : Edge_Type)
                      return Edge_Cursor'Class
   is
      --  We use a stack of edge vectors, so we can know if the cheapest is
      --  indeed over the limit.
      Best : Edge_Vectors.Vector;
      procedure Cheapest (V : Vertex_Sets.Cursor) is
         procedure Cheapest (E : Edge_Vectors.Cursor) is
            Edge : constant Edge_Cursor'Class := Edge_Vectors.Element (E);
         begin
            if
              (not V_New.Contains (Edge.First)) or else
              (not V_New.Contains (Edge.Last))
            then
               if
                 (Best.Is_Empty) or else
                 (Edge.Element /= No_Edge and then Edge < Best.First_Element)
               then
                  Best.Prepend (Edge);
               end if;
            end if;
         end Cheapest;
      begin
         Vertex_Sets.Element (V).Incident.Iterate (Cheapest'Access);
      end Cheapest;

   begin
      V_New.Iterate (Cheapest'Access);
      while
        not Best.Is_Empty and then
        not (Compound_Cost (V_New, V_Cost, Best.First_Element) < Limit)
      loop
         Log ("Cost limit hit!", Debug, Log_Section);
         Best.Delete_First;
      end loop;
      if Best.Is_Empty then
         raise Constraint_Error with "No valid edge found for MST";
      else
         return Best.First_Element;
      end if;
   end Cheapest;

   ----------
   -- Prim --
   ----------

   function Prim (G : Graph'Class) return Graph'Class is
      P : Graph'Class := G.Copy;

      V_New  : Vertex_Sets.Set;
      V_Cost : Vertex_Edge_Type_Maps.Map;
   begin
      P.Clear;

      V_New.Insert (G.First);

      while Natural (V_New.Length) < G.Vertex_Count loop
         declare
                  E : constant Edge_Cursor'Class :=
                    Cheapest (V_New, V_Cost, Infinite);
         begin
            V_New.Include (E.First);
            V_New.Include (E.Last);

            P.Insert (E.First, E.Last, E.Element, Directed => False);
         end;
      end loop;

      return P;
   end Prim;

   ----------
   -- Prim --
   ----------

   function Prim (G           : Graph'Class;
                  Root        : Vertex_Cursor'Class;
                  Depth_Limit : Edge_Type) return Graph'Class
   is
      P : Graph'Class := G.Copy;

      V_New  : Vertex_Sets.Set;
      V_Cost : Vertex_Edge_Type_Maps.Map;
   begin
      P.Clear;

      V_New.Insert  (Root);
      V_Cost.Insert (Root, Zero);

      while Natural (V_New.Length) < G.Vertex_Count loop
         declare
            E : constant Edge_Cursor'Class :=
              Cheapest (V_New, V_Cost, Depth_Limit);
         begin
            if V_New.Contains (E.First) then
               V_New.Include  (E.Last);
               V_Cost.Include (E.Last, V_Cost.Element (E.First) + E.Element);
            else
               V_New.Include  (E.First);
               V_Cost.Include (E.First, V_Cost.Element (E.Last) + E.Element);
            end if;

            P.Insert (E.First, E.Last, E.Element, Directed => False);
         end;
      end loop;

      return P;
   end Prim;

end Agpl.Containers.Graphs.Algorithms;
