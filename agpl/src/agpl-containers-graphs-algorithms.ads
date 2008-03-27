generic
   Zero     : Edge_Type;
   Infinite : Edge_Type;
   with function "+" (L, R : Edge_Type) return Edge_Type is <>;
package Agpl.Containers.Graphs.Algorithms is

   pragma Preelaborate;

   Log_Section : constant String := "agpl.containers.graphs.algorithms";

   function Prim (G : Graph'Class) return Graph'Class;

   function Prim (G           : Graph'Class;
                  Root        : Vertex_Cursor'Class;
                  Depth_Limit : Edge_Type) return Graph'Class;
   --  As previous, but we explicitly name the root and impose a depth limit from
   --  this root.

end Agpl.Containers.Graphs.Algorithms;
