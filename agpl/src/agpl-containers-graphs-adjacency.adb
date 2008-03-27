with Agpl.Text_Io;

package body Agpl.Containers.Graphs.Adjacency is

   ------------
   -- Create --
   ------------

   function Create
     (Num_Vertices : Natural)
      return Graph
   is
   begin
      return Graph'(Last_Vertex => Vertex_Index (Num_Vertices),
                    others => <>);
   end Create;

   ------------------
   -- Vertex_Count --
   ------------------

   function Vertex_Count
     (This : Graph)
      return Natural
   is
   begin
      return Natural (This.Last_Vertex);
   end Vertex_Count;

   ----------------
   -- Edge_Count --
   ----------------

   function Edge_Count
     (This : Graph)
      return Natural
   is
      Count : Natural := 0;
   begin
      for I in This.Edges'Range loop
         for J in This.Edges'Range (2) loop
            if This.Edges (I, J) /= No_Edge then
               Count := Count + 1;
            end if;
         end loop;
      end loop;
      return Count;
   end Edge_Count;

   -----------
   -- First --
   -----------

   function First (This : Graph) return Graphs.Vertex_Cursor'Class is
   begin
      pragma Unrestricted_Used ("Until extended return works in gnat");
      return Vertex_Cursor'(This'Unrestricted_Access,
                            Vertex => Vertex_Index'First);
   end First;

   ------------
   -- Insert --
   ------------

   procedure Insert (This   : in out Graph;
                     Pos    :        Graphs.Vertex_Cursor'Class;
                     Vertex :        Vertex_Type)
   is
   begin
      This.Insert (Vertex_Cursor (Pos).Vertex, Vertex);
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert (This   : in out Graph;
                     Pos    :        Vertex_Index;
                     Vertex :        Vertex_Type)
   is
   begin
      This.Vertices (Pos) := Vertex;
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (This     : in out Graph;
      Ini,
      Fin      : Graphs.Vertex_Cursor'Class;
      Edge     : Edge_Type;
      Directed : Boolean := False)
   is
   begin
      This.Insert (Vertex_Cursor (Ini).Vertex,
                   Vertex_Cursor (Fin).Vertex,
                   Edge,
                   Directed);
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (This     : in out Graph;
      Ini,
      Fin      : Vertex_Index;
      Edge     : Edge_Type;
      Directed : Boolean := False)
   is
   begin
      if
        This.Vertices (Ini) = No_Vertex or else
        This.Vertices (Fin) = No_Vertex
      then
         raise Constraint_Error with "Adding edge to undefined vertex";
      end if;

      This.Edges (Ini, Fin) := Edge;
      if not Directed then
         This.Edges (Fin, Ini) := Edge;
      end if;
   end Insert;

   ----------
   -- Copy --
   ----------

   function Copy (This : Graph) return Graph is
      G : constant Graph := This;
   begin
      return G;
   end Copy;

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Graph) is
   begin
      This.Edges := (others => (others => No_Edge));
   end Clear;

   ------------
   -- Vertex --
   ------------

   function Vertex (This : Graph;
                    I    : vertex_index) return Vertex_Cursor'Class is
   begin
      return Vertex_Cursor'(This'Unrestricted_Access, I);
   end Vertex;

   -----------
   -- Index --
   -----------

   function Index (This : Vertex_Cursor) return Vertex_Index is
   begin
      return This.Vertex;
   end Index;

   -------------
   -- Element --
   -------------

   function Element (This : Vertex_Cursor) return Vertex_Type is
   begin
      return This.Graph.Vertices (This.Vertex);
   end Element;

   ----------
   -- Next --
   ----------

   function Next (This : Vertex_Cursor) return Vertex_Cursor is
      --  One past Last_Vertex means No_Element
   begin
      if This.Vertex <= This.Graph.Last_Vertex then
         return (This.Graph, This.Vertex + 1);
      else
         raise Constraint_Error with "Vertex out of range";
      end if;
   end Next;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (This : Vertex_Cursor) return Boolean is
   begin
      return
        This.Vertex <= This.Graph.Last_Vertex and then
        This.Graph.Vertices (This.Vertex) /= No_Vertex;
   end Has_Element;

   --------------
   -- Incident --
   --------------

   function Incident (This : Vertex_Cursor) return Edge_Vector'Class is
      V : Edge_Vector;
   begin
      for I in This.Graph.Edges'Range loop
         if This.Graph.Edges (I, This.Vertex) /= No_Edge then
            V.Append (Edge_Cursor'(This.Graph, I, This.Vertex));
         end if;
         if This.Graph.Edges (This.Vertex, I) /= No_Edge then
            V.Append (Edge_Cursor'(This.Graph, This.Vertex, I));
         end if;
      end loop;
      return V;
   end Incident;

   -------------
   -- Element --
   -------------

   function Element (This : Edge_Cursor) return Edge_Type is
   begin
      return This.Graph.Edges (This.Ini, This.Fin);
   end Element;

   -----------
   -- First --
   -----------

   function First (This : Edge_Cursor) return Graphs.Vertex_Cursor'Class is
   begin
      return Vertex_Cursor'(This.Graph, This.Ini);
   end First;

   ----------
   -- Last --
   ----------

   function Last (This : Edge_Cursor) return Graphs.Vertex_Cursor'Class is
   begin
      return Vertex_Cursor'(This.Graph, This.Fin);
   end Last;

   -----------
   -- Print --
   -----------

   procedure Print (This : Graph) is
      use Agpl.Text_Io;
   begin
      for I in This.Edges'Range loop
         for J in This.Edges'Range loop
            if This.Edges (I, J) /= No_Edge then
               Put_Line (Image (This.Vertices (I)) & "(" & I'Img & ") -- " &
                         Image (This.Vertices (J)) & "(" & J'Img & "): " &
                         Image (This.Edges (I, J)));
            end if;
         end loop;
      end loop;
   end Print;

end Agpl.Containers.Graphs.Adjacency;
