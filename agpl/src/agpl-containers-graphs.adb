package body Agpl.Containers.Graphs is

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Edge_Cursor'Class) return Boolean is
   begin
      return L.Element < R.Element;
   end "<";

   ---------
   -- "<" --
   ---------

   function "<" (L, R : Vertex_Cursor'Class) return Boolean is
   begin
      return L.Element < R.Element;
   end "<";

   -----------
   -- Image --
   -----------

   function Image (This : Edge_Cursor) return String is
      F : constant Vertex_Cursor'Class := Edge_Cursor'Class (This).First;
      L : constant Vertex_Cursor'Class := Edge_Cursor'Class (This).Last;
   begin
      return Image (F.Element) & " -- " & Image (L.Element);
   end Image;

end Agpl.Containers.Graphs;
