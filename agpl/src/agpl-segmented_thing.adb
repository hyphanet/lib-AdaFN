 

--  Support for segmented things (initially implemented for segmented files).
--  Allows to keep info about an object which is integrally composed of seg-
--  ments.
--  Semantics are:
--  Equal data adjacent segments are merged automatically.
--  No position is without segment.

with Agpl.Text_IO;

package body Agpl.Segmented_Thing is

   use Ordered_Segments;
   use Ordered_Keys;
   use type Ada.Containers.Count_Type;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   --  Creates a segmented object with initially a single segment comprending
   --  all the object
   procedure Create (
      This         :    out Object;
      First        : in     Index_Type;
      Last         : in     Index_Type;
      Initial_Data : in     Segment_Data)
   is
      Inserted : Cursor;
      Success  : Boolean;
   begin
      This.First := First;
      This.Last  := Last;
      Clear (This.Segments);
      This.Pos   := 0;
      Insert (
         This.Segments,
         (Data => Initial_Data, First => First, Last => Last),
         Inserted,
         Success);
      pragma Assert (Success);
      pragma Assert (Length (This.Segments) = 1);
   end Create;

   ------------------------------------------------------------------------
   -- Count                                                              --
   ------------------------------------------------------------------------
   --  Says the number of segments.
   function Count (This : in Object) return Natural is
   begin
      return Natural (Length (This.Segments));
   end Count;

   ------------------------------------------------------------------------
   -- Indexes                                                            --
   ------------------------------------------------------------------------
   function First (This : in Object) return Index_Type is
   begin
      return This.First;
   end First;

   function Last  (This : in Object) return Index_Type is
   begin
      return This.Last;
   end Last;

   ------------------------------------------------------------------------
   -- Get                                                                --
   ------------------------------------------------------------------------
   --  Return a numbered piece
   function Get (This : in Object; Index : in Positive) return Chunk_Type is
      I : Cursor := First (This.Segments);
   begin
      pragma Assert (Index <= Natural (Length (This.Segments)));

      for K in 1 .. Index - 1 loop
         Next (I);
      end loop;
      return Element (I);
   end Get;

   --  This has variable cost: O (1) for sequential advance.
   procedure Get (
      This  : in out Object;
      Index : in     Positive;
      Data  :    out Chunk_Type)
   is
   begin
      pragma Assert (Index <= Natural (Length (This.Segments)));

      if This.Pos = 0 then
         This.Pos := 1;
         This.Idx := First (This.Segments);
      end if;

      while This.Pos < Index loop
         This.Pos := This.Pos + 1;
         Next (This.Idx);
      end loop;

      while This.Pos > Index loop
         This.Pos := This.Pos - 1;
         Previous (This.Idx);
      end loop;

      Data := Element (This.Idx);
   end Get;

   ------------------------------------------------------------------------
   -- Get_At                                                             --
   ------------------------------------------------------------------------
   --  Return the data at given point
   function Get_At (This : in Object; Pos : in Index_Type) return Segment_Data is
   begin
      pragma Assert (Pos >= This.First and then Pos <= This.Last);

      return Element (Floor (This.Segments, Pos)).Data;
   end Get_At;

   function Get_At (This : in Object; Offset, Total : in Index_Type) return Segment_Data is
   begin
      return Get_At (This,
         This.First +
         Index_Type (Float (This.Last - This.First) * Float (Offset) / Float (Total)));
   end Get_At;

   ------------------------------------------------------------------------
   -- Set                                                                --
   ------------------------------------------------------------------------
   --  Set data for a given segment. Splitting and merging are done if necessary.
   procedure Set (
      This  : in out Object;
      First : in     Index_Type;
      Last  : in     Index_Type;
      Data  : in     Segment_Data)
   is
      Success  : Boolean;
      Inserted : Cursor;
      Curr     : Cursor;
      Target   : Cursor;
      Insert_First : Index_Type := First;
      Insert_Last  : Index_Type := Last;
   begin
      if First < This.First or else Last > This.Last then
         raise Constraint_Error;
      end if;

      --  Invalidate inner cursor
      if This.Idx = Target then
         This.Pos := 0;
      end if;

      Curr := Floor (This.Segments, First);
      loop
         exit when Curr = No_Element or else Element (Curr).First > Last;
         --  Three cases: contained, overlapped or covered:
         declare
            Chunk : constant Chunk_Type := Element (Curr);
         begin
            if First <= Chunk.First and then Last >= Chunk.Last then
               --  Covered by new:
               Target := Curr;
               Next (Curr);
               Delete (This.Segments, Target);
            elsif First >= Chunk.First and then Last <= Chunk.Last then
               --  New covered by old:
               if Chunk.Data /= Data then
                  --  Carve a hole
                  declare
                     Prev_First : constant Index_Type   := Chunk.First;
                     Succ_Last  : constant Index_Type   := Chunk.Last;
                     Prev_Data  : constant Segment_Data := Chunk.Data;
                  begin
                     Delete (This.Segments, Curr);
                     if First > Prev_First then
                        Insert (
                           This.Segments,
                           (Data => Prev_Data,
                            First => Prev_First, Last => First - 1),
                           Inserted,
                           Success);
                        pragma Assert (Success);
                     end if;
                     if Last < Succ_Last then
                        Insert (
                           This.Segments,
                           (Data => Prev_Data,
                            First => Last + 1, Last => Succ_Last),
                           Inserted,
                           Success);
                        pragma Assert (Success);
                     end if;
                     exit;
                  end;
               else
                  return; -- Nothing to do
               end if;
            elsif Chunk.First < First then
               --  Overlapping right
               if Chunk.Data = Data then
                  Insert_First := Chunk.First;
                  Target       := Curr;
                  Next (Curr);
                  Delete (This.Segments, Target);
               else
                  --  Cut
                  Target := Curr;
                  Next (Curr);
                  Delete (This.Segments, Target);
                  Insert (
                     This.Segments,
                     (Data => Chunk.Data, First => Chunk.First, Last => First - 1),
                     Inserted,
                     Success);
                  pragma Assert (Success);
               end if;
            elsif Chunk.Last > Last then
               --  Overlapping left
               if Chunk.Data = Data then
                  Insert_Last  := Chunk.Last;
                  Target       := Curr;
                  Next (Curr);
                  Delete (This.Segments, Target);
               else
                  --  Cut
                  Target := Curr;
                  Next (Curr);
                  Delete (This.Segments, Target);
                  Insert (
                     This.Segments,
                     (Data => Chunk.Data, First => Last + 1, Last => Chunk.Last),
                     Inserted,
                     Success);
                  pragma Assert (Success);
               end if;
            else
               raise Program_Error; -- Should never happen
            end if;
         end;
      end loop;

      Insert (
         This.Segments,
         (Data => Data, First => Insert_First, Last => Insert_Last),
         Inserted,
         Success);
      pragma Assert (Success);
   end Set;

   ------------------------------------------------------------------------
   -- Debug_Dump                                                         --
   ------------------------------------------------------------------------
   procedure Debug_Dump (This : in Object) is
      use Text_IO;
      Curr : Cursor := First (This.Segments);
   begin
      Put_Line ("Dumping object with" & Natural'Image (Count (This)) & " segments");
      while Curr /= No_Element loop
         Put_Line (Element (Curr).First'Img & " -" & Element (Curr).Last'Img &
            ": " & Image (Element (Curr).Data));
         Next (Curr);
      end loop;
   end Debug_Dump;

   ------------------------------------------------------------------------
   -- Auxiliar functions                                                 --
   ------------------------------------------------------------------------
   function "<" (L, R : in Chunk_Type) return Boolean is
   begin
      return L.First < R.First;
   end "<";

   function "=" (L, R : in Chunk_Type) return Boolean is
   begin
      return L.First = R.First and then L.Last = R.Last and then L.Data = R.Data;
   end "=";

   function Key (This : in Chunk_Type) return Index_Type is
   begin
      return This.First;
   end Key;

   function "<" (L : in Index_Type; R : in Chunk_Type) return Boolean is
   begin
      return L < R.First;
   end "<";

   function ">" (L : in Index_Type; R : in Chunk_Type) return Boolean is
   begin
      return L > R.First;
   end ">";

end Agpl.Segmented_Thing;
