with Agpl.Strings;
with Agpl.Text_Io; use Agpl.Text_Io;

with Ada.Containers; use Ada.Containers;

package body Agpl.Graphs.Bellman_Ford is

   use Interfaces;

   use type C.Int;

   --------------
   -- Add_Edge --
   --------------

   procedure Add_Edge (This : in out Graph;
                       E    : in     Edge)
   is
      C_E : constant C_Edge := (Source => To_C  (E.Source),
                                Dest   => To_C  (E.Dest),
                                Weight => C.Int (E.Weight));
   begin
      This.C_Edges.Append (C_E);
      This.Min_Vertex := Vertex_Index'Min (This.Min_Vertex, E.Source);
      This.Min_Vertex := Vertex_Index'Min (This.Min_Vertex, E.Dest);
      This.Max_Vertex := Vertex_Index'Max (This.Max_Vertex, E.Source);
      This.Max_Vertex := Vertex_Index'Max (This.Max_Vertex, E.Dest);
   end Add_Edge;

   -------------------------
   -- Add_Undirected_Edge --
   -------------------------

   procedure Add_Undirected_Edge (This : in out Graph;
                                  E    : in     Edge)
   is
   begin
      Add_Edge (This, E);
      Add_Edge (This, Edge'(Source => E.Dest,
                            Dest   => E.Source,
                            Weight => E.Weight));
   end Add_Undirected_Edge;

   ----------------
   -- Add_Vertex --
   ----------------

   procedure Add_Vertex (This : in out Graph;
                         V    : in     Vertex)
   is
   begin
      if This.Vertices.Is_Empty or else V.Index >
         Vertex_Index (This.Vertices.Length)
      then
         This.Vertices.Set_Length (Count_Type (V.Index));
      end if;
      This.Vertices.Replace_Element (V.Index, V);

      This.Min_Vertex := Vertex_Index'Min (This.Min_Vertex, V.Index);
      This.Max_Vertex := Vertex_Index'Max (This.Max_Vertex, V.Index);
   end Add_Vertex;

   ---------------
   -- Get_Edges --
   ---------------

   function Get_Edges (This : in Graph) return Edge_Array is
      Result : Edge_Array (1 .. Integer (This.C_Edges.Length));
      Pos    : Natural := Result'First;
   begin
      for I in This.C_Edges.First_Index .. This.C_Edges.Last_Index loop
         Result (Pos) := (Source => To_Ada (This.C_Edges.Element (I).Source),
                          Dest   => To_Ada (This.C_Edges.Element (I).Dest),
                          Weight => Integer (This.C_Edges.Element (I).Weight));
         Pos := Pos + 1;
      end loop;
      return Result;
   end Get_Edges;

   ----------------
   -- Get_Vertex --
   ----------------

   function Get_Vertex (This  : in Graph;
                        Index : in Vertex_Index) return Vertex is
   begin
      return This.Vertices.Element (Index);
   end Get_Vertex;

   ------------------
   -- Get_Vertices --
   ------------------

   function Get_Vertices (This : in Graph) return Vertex_Vectors.Vector is
   begin
      return This.Vertices;
   end Get_Vertices;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (This : in Graph) return Boolean is
      Min : constant Vertex_Index := Min_Vertex (This);
      Max : constant Vertex_Index := Max_Vertex (This);
      Used : array (Min .. Max) of Boolean;
   begin
      if Used'First /= Vertex_Index'First then
         Put_Line ("First vertex is not" & Vertex_Index'First'Img);
         return False;
      else
         for I in This.C_Edges.First_Index .. This.C_Edges.Last_Index loop
            Used (To_Ada (This.C_Edges.Element (I).Source)) := True;
            Used (To_Ada (This.C_Edges.Element (I).Dest)) := True;
         end loop;

         for I in Used'Range loop
            if not Used (I) then
               Put_Line ("There are unlinked vertices");
               return False;
            end if;
         end loop;
         return True;
      end if;
   end Is_Valid;
   --  Verify the sanity of a Graph.

   -----------------------
   -- Costs_From_Source --
   -----------------------

   function Costs_From_Source (This   : in Graph;
                               Source : in Vertex_Index) return Cost_Array
   is
      Gr   : constant C_Edge_Array := To_C_Edge_Array (This);
      Dist :          C_Cost_Array (0 .. C.Int (To_C (Max_Vertex (This))));
   begin
      Bellman_Ford (Gr,
                    Source       => To_C (Source),
                    Vertex_Count => C.Int (Max_Vertex (This)),
                    Distance     => Dist);

      declare
         Result : Cost_Array (1 .. Dist'Length);
         I      : C.Int := Dist'First;
      begin
         for J in Result'Range loop
            Result (J) := Integer (Dist (I));
            I := I + 1;
         end loop;
         return Result;
      end;
   end Costs_From_Source;

   -------------------
   -- Compute_Costs --
   -------------------

   procedure Compute_Costs (This : in out Graph) is
   begin
      This.Costs.Set (Get_Costs (This, Cached => False));
   end Compute_Costs;

   ---------------
   -- Get_Costs --
   ---------------

   function Get_Costs (This   : in Graph;
                       Cached : in Boolean := True) return Cost_Matrix is
      Result : Cost_Matrix :=
                 Cost_Matrices.Create (Max_Vertex (This),
                                       Max_Vertex (This));
   begin
      if Cached and then This.Costs.Is_Valid then
         return This.Costs.Get;
      end if;

      --  Put_Line ("Computing graph costs...");
      for I in Result.First_Row .. Result.Last_Row loop
         declare
            Row : constant Cost_Array := Costs_From_Source (This, I);
         begin
            pragma Assert (Row'First = Result.First_Col);
            for J in Result.First_Col .. Result.Last_Col loop
               Result.Set (I, J, Row (J));
            end loop;
         end;
      end loop;

--        Put_Line ("C GRAPH");
--        Print_C_Graph (To_C_Edge_Array (This));
--        Put_Line ("C GRAPH END");

      return Result;
   end Get_Costs;

   ------------------
   -- Bellman_Ford --
   ------------------

   procedure Bellman_Ford (Graph        : in C_Edge_Array;
                           Source       : in C_Vertex;
                           Vertex_Count : in C.Int;
                           Distance     :    out C_Cost_Array)
   is
      procedure Bf_C (Graph        : in C_Edge_Array;
                      Edge_Count   : in C.Int;
                      Vertex_Count : in C.Int;
                      Source       : in C_Vertex;
                      Distance     :    out C_Cost_Array);
      pragma Import (C, Bf_C, "BellmanFord");
   begin
      Bf_C (Graph,
            Edge_Count   => Graph'Length,
            Vertex_Count => Vertex_Count,
            Source       => Source,
            Distance     => Distance);
   end Bellman_Ford;

   ----------
   -- To_C --
   ----------

   function To_C   (This : in Vertex_Index) return C_Vertex is
   begin
      return C_Vertex (This - 1);
   end To_C;

   ------------
   -- To_Ada --
   ------------

   function To_Ada (This : in C_Vertex) return Vertex_Index is
   begin
      return Vertex_Index (This + 1);
   end To_Ada;

   ----------------
   -- Min_Vertex --
   ----------------

   function Min_Vertex (This : in Graph) return Vertex_Index is
   begin
      return This.Min_Vertex;
   end Min_Vertex;

   ----------------
   -- Max_Vertex --
   ----------------

   function Max_Vertex (This : in Graph) return Vertex_Index is
   begin
      return This.Max_Vertex;
   end Max_Vertex;

   ---------------------
   -- To_C_Edge_Array --
   ---------------------

   function To_C_Edge_Array (This : in Graph) return C_Edge_Array is
      Result : C_Edge_Array (0 .. C.Int (This.C_Edges.Last_Index));
   begin
      for I in Result'Range loop
         Result (I) := C_Edge_Vectors.Element (This.C_Edges, Natural (I));
      end loop;
      return Result;
   end To_C_Edge_Array;

   -------------------
   -- Print_C_Graph --
   -------------------

   procedure Print_C_Graph (This : in C_Edge_Array) is
   begin
      for I in This'Range loop
         Put_Line (This (I).Source'Img &
                   This (I).Dest'Img &
                   This (I).Weight'Img);
      end loop;
   end Print_C_Graph;

   ------------------
   -- Test_Package --
   ------------------

   procedure Test_Package is
      G : Graph;

      Edges : constant Edge_Array :=
               ((1, 2, 1),
                (2, 3, 2),
                (1, 4, 1),
                (4, 3, 1),
                (4, 5, 3),
                (3, 5, 5));

      use Agpl.Strings;
   begin
      for I in Edges'Range loop
         G.Add_Undirected_Edge (Edges (I));
      end loop;

      pragma Assert (Is_Valid (G));

      Put_Line ("Min. Vertex is" & G.Min_Vertex'Img);
      Put_Line ("Max. Vertex is" & G.Max_Vertex'Img);

      declare
         C : constant Cost_Matrix := G.Get_Costs;
      begin
         pragma Assert (C.Last_Row = G.Max_Vertex);
         pragma Assert (C.Last_Col = G.Max_Vertex);
         for I in C.First_Row .. C.Last_Row loop
            for J in C.First_Col .. C.Last_Col loop
               Put (Rpad (Trim (To_String (C.Get (I, J))), 2));
            end loop;
            New_Line;
         end loop;
      end;
   end Test_Package;

   ------------------
   -- Is_Connected --
   ------------------

   function Is_Connected (This : in Graph) return Boolean is
      C : constant Cost_Matrix := This.Get_Costs;
   begin
      for Row in C.First_Row .. C.Last_Row loop
         for Col in C.First_Col .. C.Last_Col loop
            if C.Get (Row, Col) > 1_000_000_000 then
               Put_Line ("Too big cost:" & C.Get (Row, Col)'Img);
               return False;
            end if;
         end loop;
      end loop;

      return True;
   end Is_Connected;

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Graph) is
   begin
      This.C_Edges.Clear;
      This.Vertices.Clear;
      This.Min_Vertex := Vertex_Index'Last;
      This.Max_Vertex := Vertex_Index'First;
      This.Costs.Clear;
   end Clear;

end Agpl.Graphs.Bellman_Ford;
