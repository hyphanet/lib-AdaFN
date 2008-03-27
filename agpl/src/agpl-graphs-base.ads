package Agpl.Graphs.Base is

   pragma Preelaborate;

   type Object is abstract tagged private;

   function Vertex_Count (This : Object) return Natural is abstract;

   function Edge_Count (This : Object) return Natural is abstract;

private

   type Object is abstract tagged null record;

end Agpl.Graphs.Base;
