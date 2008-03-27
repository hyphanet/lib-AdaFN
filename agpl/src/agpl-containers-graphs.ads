with Ada.Containers.Indefinite_Vectors,
     Ada.Containers.Indefinite_Ordered_Maps,
     Ada.Containers.Indefinite_Ordered_Sets,
     Agpl.Generic_Handle;

generic
   type Vertex_Type is private;
   No_Vertex : Vertex_Type;
   type Edge_Type   is private;
   No_Edge   : Edge_Type; -- Used to signal infinite length or missing
   with function "<" (L, R : Vertex_Type) return Boolean is <>;
   with function Image (V : Vertex_Type) return String is <>;
   with function "<" (L, R : Edge_Type) return Boolean is <>;
   with function Image (E : Edge_Type) return String is <>;
   --  Weight or something, must be true for all E < No_Edge
package Agpl.Containers.Graphs is

   pragma Preelaborate;

   type Graph is abstract tagged private;

   --  type Edge_Precursor is abstract tagged null record;
   --  Used only to have edge vectors of this class.
   --  Real instances will always be at least in Edge_Cursor'Class

   type Edge_Cursor is abstract tagged null record;

   type Vertex_Cursor is abstract tagged null record;

   --  GRAPH PRIMITIVES

   function Vertex_Count (This : Graph) return Natural is abstract;

   function Edge_Count (This : Graph) return Natural is abstract;

   function First (This : Graph) return Vertex_Cursor'Class is abstract;

   procedure Insert (This   : in out Graph;
                     Pos    :        Vertex_Cursor'Class;
                     Vertex :        Vertex_Type) is abstract;

   procedure Insert (This     : in out Graph;
                     Ini,
                     Fin      : Vertex_Cursor'Class;
                     Edge     : Edge_Type;
                     Directed : Boolean := False) is abstract;

   function Copy (This : Graph) return Graph is abstract;
   --  Copy creator

   procedure Clear (This : in out Graph) is abstract;


   --  VERTEX PRIMITIVES

   function Element (This : Vertex_Cursor) return Vertex_Type is abstract;

   function Next (This : Vertex_Cursor) return Vertex_Cursor is abstract;

   function Has_Element (This : Vertex_Cursor) return Boolean is abstract;

   --  EDGE PRIMITIVES

   function Element (This : Edge_Cursor) return Edge_Type is abstract;

   function First (This : Edge_Cursor) return Vertex_Cursor'Class is abstract;

   function Last (This : Edge_Cursor) return Vertex_Cursor'Class is abstract;

   function Image (This : Edge_Cursor) return String;

   function "<" (L, R : Edge_Cursor'Class) return Boolean;
   --  Uses elements at each for comparison

      --  CONTAINERS

   function "<" (L, R : Vertex_Cursor'Class) return Boolean;
   --  Uses elements at each for comparison

   package Edge_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, Edge_Cursor'Class);
   type Edge_Vector is new Edge_Vectors.Vector with null record;

   function Incident (This : Vertex_Cursor) return Edge_Vector'Class
                      is abstract;

   package Vertex_Sets is
     new Ada.Containers.Indefinite_Ordered_Sets (Vertex_Cursor'Class);
   type Vertex_Set is new Vertex_Sets.Set with null record;

   package Vertex_Edge_Type_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (Vertex_Cursor'Class,
                                                 Edge_Type);

   package Edge_Handles is new Agpl.Generic_Handle (Edge_Cursor'Class);
   type Edge_Handle is new Edge_Handles.Object with null record;

private

   type Graph is abstract tagged null record;

end Agpl.Containers.Graphs;
