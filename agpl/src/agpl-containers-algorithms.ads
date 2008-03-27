with Ada.Containers;

package Agpl.Containers.Algorithms is

   --  Extra algorithms for containers
   pragma Preelaborate;

   Log_Section : constant String := "agpl.containers.algorithms";

   generic
      type Container is private;
      type Element_Type (<>) is private;
      type Cursor is private;
--      with function "="     (L, R : Element_Type) return Boolean is <>;
      with function Element (Position : Cursor) return Element_Type is <>;
   package Basic is

      generic
         with procedure Append
           (Cont      : in out Container;
            New_Item  :        Element_Type;
            Count     :        Ada.Containers.Count_Type := 1) is <>;
         with procedure Iterate
           (Cont    : Container;
            Process : not null access procedure (Position : Cursor)) is <>;
      procedure Append (Dst : in out Container;
                        Src :        Container);

      generic
         with procedure Prepend
           (Cont      : in out Container;
            New_Item  :        Element_Type;
            Count     :        Ada.Containers.Count_Type := 1) is <>;
         with procedure Iterate
           (Cont    : Container;
            Process : not null access procedure (Position : Cursor)) is <>;
      function Invert (Cont : Container) return Container;
      --  Reverse is a reserved keyword

      generic
         with function Has_Element (Position : Cursor) return Boolean is <>;
         with function First       (Cont : Container) return Cursor is <>;
         with function Last        (Cont : container) return Cursor is <>;
         with function Next        (Position : Cursor) return Cursor is <>;
         with function Previous    (Position : Cursor) return Cursor is <>;
         with procedure Append
           (Cont      : in out Container;
            New_Item  :        Element_Type;
            Count     :        Ada.Containers.Count_Type := 1) is <>;
      package Fields is

         --  Getting parts of a container

         function Slice (Cont : Container;
                         Ini,
                         Fin  : Element_Type;
                         Pos  : Positive := 1) return Container;
         --  If Ini or Fin don't appear in the matching position, empty result.

         function Tail (Cont : Container;
                        Sep  : Element_Type;
                        Pos  : Positive := 1) return Container;
         --  The remainder of a Container starting at Separator, counting Pos from end.
         --  Separator obviously not included.

      end Fields;

   end Basic;

end Agpl.Containers.Algorithms;
