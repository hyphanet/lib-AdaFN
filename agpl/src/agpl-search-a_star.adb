with Ada.Containers.Ordered_Multisets,
     Ada.Containers.Indefinite_Ordered_Maps,
     Agpl.Containers.String_Sets,
     Agpl.Trace,
     System.Pool_Local;

use Agpl.Trace;

package body Agpl.Search.A_Star is

   ---------------
   -- Best_Path --
   ---------------

   procedure Best_Path (Ini,
                        Fin   :     State;
                        Route : out Path;
                        Cost  : out Costs)
   is
      Local_Pool : System.Pool_Local.Unbounded_Reclaim_Pool;

      type Node;
      type Node_Access is access Node;
      for Node_Access'Storage_Pool use Local_Pool; -- Automatic reclaim

      type Node is record
         Cost : Costs; -- Includes real + estimation, for sorting in pending
         Real : Costs; -- The real cost till Curr without estimation
         Prev : Node_Access;
         Curr : State;
         Len  : Positive;
      end record;

      function Image (N : Node) return String is
      begin
         return Image (N.Curr) & N.Len'Img & Image (N.Real) & Image (N.Cost);
      end Image;

      ---------
      -- "<" --
      ---------

      function "<" (L, R : Node_Access) return Boolean is
      begin
         return L.Cost < R.Cost;
      end "<";

      package Node_Sets  is new Ada.Containers.Ordered_Multisets (Node_Access);

      package String_Cost_Maps is new
        Ada.Containers.Indefinite_Ordered_Maps (String, Costs);

      ----------------
      -- Build_Path --
      ----------------

      procedure Build_Path (Fin : Node_Access) is
         I   : Node_Access := Fin;
      begin
         Cost := Fin.Cost;

         loop
            Prepend (Route, I.Curr);
            exit when I.Prev = null;
            I := I.Prev;
         end loop;
      end Build_Path;

      Pending    : Node_Sets.Set;
      --  Visited    : Containers.String_Sets.Set;
      Candids    : String_Cost_Maps.Map;
      Success    : Boolean  := False;
      Iter       : Positive := 1;
   begin
      Pending.Insert (new Node'(Zero, Zero, null, Ini, 1));
      Candids.Insert (Image (Pending.First_Element.Curr), Zero);

      while not Pending.Is_Empty loop
         Log ("::" & Iter'Img, Debug, Log_Section);
         Iter := Iter + 1;
         declare
            Curr : constant Node_Access := Pending.First_Element;
         begin
            Pending.Delete_First;
            Log ("At node " & Image (Curr.all), Debug, Log_Section);
            if Image (Curr.Curr) = Image (Fin) then
               Log ("Fin reached", Debug, Log_Section);
               Build_Path (Curr);
               Success := True;
               exit;
            else
               for I in 1 .. Num_Next (Curr.Curr) loop
                  declare
                     S : constant State := Next      (Curr.Curr, I);
                     G : constant Costs := Real_Cost (Curr.Curr, S);
                     H : constant Costs := Estimate  (S,         Fin);
                     N : constant Node  := (Curr.Real + G + H,
                                            Curr.Real + G,
                                            Curr,
                                            S,
                                            Curr.Len + 1);
                  begin
                     if
                       (not Candids.Contains (Image (S))) or else
                       N.Cost < Candids.Element (Image (S))
                     then
                        Log ("Added neighbor #" & I'Img & ":" & Image (N),
                             Debug, Log_Section);
                        Candids.Include (Image (S), N.Cost);
                        Pending.Insert (new Node'(N));
                     else
                        Log ("Rejected neighbor #" & I'Img, Debug, Log_Section);
                     end if;
                  end;
               end loop;
            end if;
         end;
      end loop;

      if not Success then
         raise Constraint_Error with
         "No A* route: " & Image (Ini) & " --> " & Image (Fin);
      end if;

   end Best_Path;

end Agpl.Search.A_Star;
