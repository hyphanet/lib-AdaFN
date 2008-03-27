with Agpl.Trace;

use Agpl.Trace;

package body Agpl.Containers.Algorithms is

   -----------
   -- Basic --
   -----------

   package body Basic is

      ------------
      -- Append --
      ------------

      procedure Append
        (Dst : in out Container;
         Src :        Container)
      is
         procedure Append (I : Cursor) is
         begin
            Append (Dst, Element (I));
         end Append;
      begin
         Iterate (Src, Append'Access);
      end Append;

      ------------
      -- Invert --
      ------------

      function Invert (Cont : Container) return Container is
         Dst : Container;

         procedure Invert (I : Cursor) is
         begin
            Prepend (Dst, Element (I));
         end Invert;
      begin
         Iterate (Cont, Invert'Access);
      end Invert;

      ------------
      -- Fields --
      ------------

      package body Fields is

         ---------------
         -- Copy_From --
         ---------------

         function Copy_From (I : Cursor) return Container is
            Cont : Container;
            J    : Cursor := I;
         begin
            while Has_Element (J) loop
               Append (Cont, Element (J));
               J := Next (J);
            end loop;
            return Cont;
         end Copy_From;

         -----------
         -- Slice --
         -----------

         function Slice (Cont : Container;
                         Ini,
                         Fin  : Element_Type;
                         Pos  : Positive := 1) return Container
         is
            Seen   : Natural := 0;
            I      : Cursor  := First (Cont);
            Result : Container;
            Empty  : Container;
         begin
            while Has_Element (I) loop
               if Element (I) = Ini then
                  Seen := Seen + 1;
                  exit when Seen = Pos;
               end if;
               I := Next (I);
            end loop;

            if Seen = 0 then
               return Empty;
            end if;

            Seen := 0;

            while Has_Element (I) loop
               Append (Result, Element (I));
               if Element (I) = Fin then
                  Seen := 1;
                  exit;
               end if;
               I := Next (I);
            end loop;

            if Seen = 0 then
               return Empty;
            end if;

            return Result;
         end Slice;

         ----------
         -- Tail --
         ----------

         function Tail (Cont : Container;
                        Sep  : Element_Type;
                        Pos  : Positive := 1) return Container
         is
            Seen : Natural := 0;
            I    : Cursor  := Last (Cont);
         begin
            --  Log ("tail entered", Always);
            while Has_Element (I) loop
               if Element (I) = Sep then
                  --  Log ("separator found", Always);
                  Seen := Seen + 1;
                  if Seen = Pos then
                     return Copy_From (Next (I));
                  end if;
               end if;
               I := Previous (I);
            end loop;
            Log ("Separator not found", Debug, Log_Section);
            return Cont;
         end Tail;

      end Fields;

   end Basic;

end Agpl.Containers.Algorithms;
