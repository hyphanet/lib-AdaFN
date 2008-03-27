with Agpl.Text_Io,
     Agpl.Containers.Graphs,
     Agpl.Containers.Graphs.Adjacency,
     Agpl.Containers.Graphs.Algorithms;

use Agpl.Text_Io;

procedure T023_Prim is

   package Wg is new Agpl.Containers.Graphs (Positive,
                                             Positive'Last,
                                             Natural,
                                             Natural'Last,
                                             "<", Positive'Image,
                                             "<", Natural'Image);

   package Adj is new Wg.Adjacency;
   package Alg is new Wg.Algorithms (0, Natural'Last, "+");

   G : Adj.Graph := Adj.Create (3);

begin
   G.Insert (1, 1);
   G.Insert (2, 2);
   G.Insert (3, 3);

   G.Insert (1, 2, 2);
   G.Insert (1, 3, 2);
   G.Insert (2, 3, 3);

   Put_Line ("G");
   G.Print;

   declare
      P : constant Adj.Graph := Adj.Graph (Alg.Prim (G));
   begin
      New_Line;
      Put_Line ("P");
      P.Print;
   end;
end T023_Prim;
