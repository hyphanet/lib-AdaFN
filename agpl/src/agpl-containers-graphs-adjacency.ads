generic
package Agpl.Containers.Graphs.Adjacency is

   --  Graph implementation using adjacency matrices

   pragma Preelaborate;

   type Graph (<>) is new Graphs.Graph with private;

   type Vertex_Cursor (<>) is new Graphs.Vertex_Cursor with private;

   type Edge_Cursor (<>) is new Graphs.Edge_Cursor with private;

   type Vertex_Index is new Positive;

   --  GRAPH PRIMITIVES

   not overriding
   function Create (Num_Vertices : Natural) return Graph;

   overriding
   function Vertex_Count (This : Graph) return Natural;
   --  O (1)

   overriding
   function Edge_Count (This : Graph) return Natural;
   --  O (v^2)

   function First (This : Graph) return Graphs.Vertex_Cursor'Class;

   procedure Insert (This   : in out Graph;
                     Pos    :        Graphs.Vertex_Cursor'Class;
                     Vertex :        Vertex_Type);

   procedure Insert (This   : in out Graph;
                     Pos    :        Vertex_Index;
                     Vertex :        Vertex_Type);

   procedure Insert (This     : in out Graph;
                     Ini,
                     Fin      : Graphs.Vertex_Cursor'Class;
                     Edge     : Edge_Type;
                     Directed : Boolean := False);

   procedure Insert (This     : in out Graph;
                     Ini,
                     Fin      : Vertex_Index;
                     Edge     : Edge_Type;
                     Directed : Boolean := False);

   function Copy (This : Graph) return Graph;

   procedure Clear (This : in out Graph);

   procedure Print (This : Graph);

   --  VERTEX PRIMITIVES

   function Vertex (This : Graph; I : Vertex_Index) return Vertex_Cursor'Class;
   function Cursor (This : Graph; I : Vertex_Index) return Vertex_Cursor'Class
                    renames Vertex;

   function Index (This : Vertex_Cursor) return Vertex_Index;

   function Element (This : Vertex_Cursor) return Vertex_Type;

   function Next (This : Vertex_Cursor) return Vertex_Cursor;

   function Has_Element (This : Vertex_Cursor) return Boolean;

   function Incident (This : Vertex_Cursor) return Edge_Vector'Class;

   --  EDGE PRIMITIVES

   function Element (This : Edge_Cursor) return Edge_Type;

   function First (This : Edge_Cursor) return Graphs.Vertex_Cursor'Class;

   function Last (This : Edge_Cursor) return Graphs.Vertex_Cursor'Class;

private

   type Graph_Access is access all Graph;

   type Vertex_Array is array (Vertex_Index range <>) of Vertex_Type;

   type Edge_Matrix is array (Vertex_Index range <>,
                              Vertex_Index range <>) of Edge_Type;

   type Graph (Last_Vertex : Vertex_Index) is new Graphs.Graph with record
      Vertices : Vertex_Array
        (Vertex_Index'First .. Last_Vertex) := (others => No_Vertex);
      Edges    : Edge_Matrix
        (Vertex_Index'First .. Last_Vertex,
         Vertex_Index'First .. Last_Vertex) := (others => (others => No_Edge));
   end record;

   type Vertex_Cursor (Graph : Graph_Access) is new Graphs.Vertex_Cursor with
      record
         Vertex : Vertex_Index;
      end record;

   type Edge_Cursor (Graph : Graph_Access) is new Graphs.Edge_Cursor with
      record
         Ini,
         Fin : Vertex_Index;
      end record;

end Agpl.Containers.Graphs.Adjacency;
