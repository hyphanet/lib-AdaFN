with Ada.Containers.Indefinite_Ordered_Multisets,
     Ada.Unchecked_Deallocation,
     Agpl.Text_Io;

with Gnat.Debug_Utilities;

package body Agpl.Containers.Unbounded_Trees is

   procedure Free is new Ada.Unchecked_Deallocation (Node, Node_Access);

   function Image (Node : Node_Data) return String is
   begin
      return Gnat.Debug_Utilities.Image (Node'Address);
   end Image;

   function Image (Node : Child_Index) return String is
      pragma Unreferenced (Node);
   begin
      return "&";
   end Image;

   -----------------
   -- Null_Cursor --
   -----------------

   function Null_Cursor return Cursor is
   begin
      return Bind (new Precursor'(True, null, null, null));
   end Null_Cursor;

   -------------------
   -- Child_Or_Null --
   -------------------

   function Child_Or_Null (Node  : Node_Access;
                           Which : Child_Index) return Node_Access
   is
      use Node_Maps;
   begin
      if Node = null then
         return null;
      else
         declare
            Child : constant Node_Maps.Cursor := Node.Children.Find (Which);
         begin
            if Has_Element (Child) then
               return Element (Child);
            else
               return null;
            end if;
         end;
      end if;
   end Child_Or_Null;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (This : Cursor) return Boolean is
   begin
      return This.Ref.Current /= null;
   end Has_Element;

   ------------------
   -- Has_Position --
   ------------------

   function Has_Position (This : Cursor) return Boolean is
   begin
      return This.Ref.Root /= null; -- Root is null for the Null_Cursor
   end Has_Position;

   ------------------
   -- Has_Children --
   ------------------

   function Has_Children (This : Cursor) return Boolean is
   begin
      return not This.Ref.Current.Children.Is_Empty;
   end Has_Children;

   -------------
   -- Is_Root --
   -------------

   function Is_Root (This : Cursor) return Boolean is
   begin
      return This.Ref.Is_Root;
   end Is_Root;

   -----------
   -- Index --
   -----------

   function Index (This : Cursor) return Child_Index is
   begin
      return +This.Ref.Current.Index;
   end Index;

   -------------
   -- Is_Leaf --
   -------------

   function Is_Leaf (This : Cursor) return Boolean is
   begin
      return not This.First_Child.Has_Element;
   end Is_Leaf;

   ----------
   -- Root --
   ----------

   function Root (This : Tree) return Cursor'Class is
   begin
      return
        Bind
          (new Precursor'
               (Is_Root => True,
                Root    => This.This,
                Parent  => null,
                Current => This.Root));
   end Root;

   ----------
   -- Last --
   ----------

   function Last (This : Tree) return Cursor'Class is
      I : Cursor := Cursor (Root (This));
   begin
      while I.Has_Element loop
         I := I.Last_Child;
      end loop;
      return I;
   end Last;

   ----------
   -- Root --
   ----------

   function Root (This : Cursor) return not null access Tree'Class is
   begin
      return This.Ref.Root;
   end Root;

   ----------
   -- Root --
   ----------

   function Root (This : Cursor) return Cursor is
   begin
      return Cursor (Root (This.Ref.Root.all));
   end Root;

   -------------
   -- Element --
   -------------

   function Element (This : Cursor) return Node_Data is
   begin
      return +This.Ref.Current.Data;
   end Element;

   ------------
   -- Update --
   ------------

   function Update (This : Cursor) return not null access Node_Data is
   begin
      return This.Ref.Current.Data.Ref;
   end Update;

   -----------
   -- Query --
   -----------

   function Query (This : Cursor) return not null access constant Node_Data is
   begin
      return This.Ref.Current.Data.Ref;
   end Query;

   ------------
   -- Parent --
   ------------

   function Parent (This : Cursor) return Cursor is
   begin
      if This.Ref.Is_Root then
         raise Constraint_Error with "Root has no parent";
      elsif This.Ref.Parent.Is_Root then
         return
           Bind (new Precursor'(Is_Root        => True,
                                Root           => This.Ref.Root,
                                Parent         => This.Ref.Parent.Parent,
                                Current        => This.Ref.Parent));
      else
         return
           Bind (new Precursor'(Is_Root        => False,
                                Root           => This.Ref.Root,
                                Parent         => This.Ref.Parent.Parent,
                                Current        => This.Ref.Parent,
                                Index          => This.Ref.Parent.Index));
      end if;
   end Parent;

   -----------
   -- Child --
   -----------

   function Child
     (This  : Cursor;
      Which : Child_Index)
      return Cursor
   is
      pragma Bug_Workaround ("Making this without intermediate var bombs");
      C : constant Cursor :=
            Bind
              (new Precursor'
                 (Is_Root        => False,
                  Root           => This.Ref.Root,
                  Parent         => This.Ref.Current,
                  Current        => Child_Or_Null (This.Ref.Current, Which),
                  Index          => +Which));
   begin
      return C;
   end Child;

   -----------------
   -- First_Child --
   -----------------

   function First_Child (This : Cursor) return Cursor is
   begin
      if This.Ref.Current.Children.Is_Empty then
         return Null_Cursor;
      else
         return
           Bind
             (new Precursor'
               (Is_Root        => False,
                Root           => This.Ref.Root,
                Parent         => This.Ref.Current,
                Current        => This.Ref.Current.Children.First_Element,
                Index          => +Node_Maps.Key
                  (This.Ref.Current.Children.First)));
      end if;
   end First_Child;

   ----------------
   -- Last_Child --
   ----------------

   function Last_Child (This : Cursor) return Cursor is
   begin
      if This.Ref.Current.Children.Is_Empty then
         return Null_Cursor;
      else
         return
           Bind
             (new Precursor'
               (Is_Root        => False,
                Root           => This.Ref.Root,
                Parent         => This.Ref.Current,
                Current        => This.Ref.Current.Children.Last_Element,
                Index          => +Node_Maps.Key
                  (This.Ref.Current.Children.Last)));
      end if;
   end Last_Child;

   ----------------------
   -- Previous_Sibling --
   ----------------------

   function Previous_Sibling (This : Cursor) return Cursor is
      use Node_Maps;
   begin
      if This.Ref.Is_Root then
         raise Constraint_Error with "Root node has no siblings";
      elsif not This.Has_Position then
         raise Constraint_Error with "Unpositioned cursor has no siblings";
      else
         if This.Ref.Index.Get = Key (This.Ref.Parent.Children.First) then
            return Null_Cursor;
         else
            declare
               Self : constant Node_Maps.Cursor :=
                        This.Ref.Parent.Children.Find (+This.Ref.Index);
            begin
               return
                 Bind
                   (new Precursor'
                        (Is_Root        => False,
                         Root           => This.Ref.Root,
                         Parent         => This.Ref.Parent,
                         Current        => Element (Previous (Self)),
                         Index          => +Key    (Previous (Self))));
            end;
         end if;
      end if;
   end Previous_Sibling;

   ------------------
   -- Next_Sibling --
   ------------------

   function Next_Sibling (This : Cursor) return Cursor is
      use Node_Maps;
   begin
      if This.Ref.Is_Root then
         raise Constraint_Error with "Root node has no siblings";
      elsif not This.Has_Position then
         raise Constraint_Error with "Unpositioned cursor has no siblings";
      else
         if This.Ref.Index.Get = Key (This.Ref.Parent.Children.Last) then
            return Null_Cursor;
         else
            declare
               Self : constant Node_Maps.Cursor :=
                        This.Ref.Parent.Children.Find (+This.Ref.Index);
            begin
               return
                 Bind
                   (new Precursor'
                        (Is_Root        => False,
                         Root           => This.Ref.Root,
                         Parent         => This.Ref.Parent,
                         Current        => Element (Next (Self)),
                         Index          => +Key    (Next (Self))));
            end;
         end if;
      end if;
   end Next_Sibling;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (This : Cursor;
      Data : Node_Data)
   is
   begin
      if This.Ref.Current /= null then
         raise Constraint_Error with "Node already exists in Insert";
      elsif not This.Has_Position then
         raise Constraint_Error with "Cursor has no position";
      end if;

      if
      not This.Ref.Is_Root and then
        This.Ref.Parent.Children.Contains (+This.Ref.Index)
      then
         raise Constraint_Error with "Child already exists in parent node";
      end if;
      if This.Ref.Is_Root then
         pragma Assert (This.Ref.Parent    = null);
         pragma Assert (This.Ref.Root.Root = null);
         This.Ref.Current :=
           new Node'(Is_Root        => True,
                     Root           => This.Ref.Root,
                     Parent         => This.Ref.Parent,
                     Data           => +Data,
                     Children       => <>);
         This.Ref.Root.Root := This.Ref.Current;
      else
         This.Ref.Current :=
           new Node'(Is_Root        => False,
                     Root           => This.Ref.Root,
                     Parent         => This.Ref.Parent,
                     Data           => +Data,
                     Index          => This.Ref.Index,
                     Children       => <>);
         This.Ref.Parent.Children.Insert (+This.Ref.Index, This.Ref.Current);
      end if;
   end Insert;

   -------------
   -- Include --
   -------------

   procedure Include
     (This : Cursor;
      Data : Node_Data)
   is
   begin
      if This.Ref.Current /= null then
         Replace_Data (This.Ref.Current.all, Data);
      else
         This.Insert (Data);
      end if;
   end Include;

   ------------------
   -- Replace_Data --
   ------------------

   procedure Replace_Data (This : in out Node;
                           Data :        Node_Data)
   is
   begin
      This.Data.Set (Data);
   end Replace_Data;

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Node_Access) is
      procedure Clear (I : Node_Maps.Cursor) is
         Child : Node_Access := Node_Maps.Element (I);
      begin
         Clear (Child);
      end Clear;
   begin
      if This /= null then
         This.Children.Iterate (Clear'Access);
         Free (This);
      end if;
   end Clear;

   -----------
   -- Clear --
   -----------

   procedure Clear (This : Cursor) is
   begin
      Clear (This.Ref.all.Current);
      if This.Ref.Is_Root then
         This.Ref.Root.Root := null;
      end if;
   end Clear;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (This  : Tree;
      Query : not null access procedure (I : Cursor))
   is
      procedure Iterate (This : Node_Access) is
         procedure Iterate (I : Node_Maps.Cursor) is
         begin
            Iterate (Node_Maps.Element (I));
         end Iterate;
      begin
         if This /= null then
            Query (Make_Cursor (This));
            This.Children.Iterate (Iterate'Access);
         end if;
      end Iterate;
   begin
      Iterate (This.Root);
   end Iterate;

   ----------------------
   -- Iterate_Children --
   ----------------------

   procedure Iterate_Children
     (This  : Cursor;
      Query : not null access procedure (I : Cursor))
   is
      use Node_Maps;
      procedure Iterate_Children (I : Node_Maps.Cursor) is
      begin
         Query (Make_Cursor (Element (I)));
      end Iterate_Children;
   begin
      This.Ref.Current.Children.Iterate (Iterate_Children'Access);
   end Iterate_Children;

   ------------------------------
   -- Iterate_Ordered_Children --
   ------------------------------

   procedure Iterate_Ordered_Children
     (This  : Cursor;
      Query : not null access procedure (I : Cursor))
   is
      package Sets is
        new Ada.Containers.Indefinite_Ordered_Multisets (Cursor, Precedes);

      Set : Sets.Set;

      procedure Iterate_Ordered_Children_Build_Map (I : Node_Maps.Cursor) is
      begin
         Set.Insert (Make_Cursor (Node_Maps.Element (I)));
      end Iterate_Ordered_Children_Build_Map;

      procedure Iterate_Ordered_Children_Query (I : Sets.Cursor) is
      begin
         Query (Sets.Element (I));
      end Iterate_Ordered_Children_Query;
   begin
      declare
         pragma Bug_Workaround;
         --  Without this stupid block, this function bombs out at exit,
         --    when Set is being automatically finalized.
         Test_Set : Sets.Set;
      begin
         Test_Set.Insert (This);
      end;

      if
        This.Ref.Current /= null and then
        not This.Ref.Current.Children.Is_Empty
      then
         This.Ref.Current.Children.Iterate
           (Iterate_Ordered_Children_Build_Map'Access);
         Set.Iterate
           (Iterate_Ordered_Children_Query'Access);
      end if;
   end Iterate_Ordered_Children;

   -----------
   -- Print --
   -----------

   procedure Print (This : Tree) is
      use Agpl.Text_Io;
      procedure Print (This : Node_Access; Depth : Natural := 0) is
         function Prefix return String is
         begin
            return String'(1 .. Depth => '-');
         end Prefix;
         procedure Print (I : Node_Maps.Cursor) is
         begin
            Print (Node_Maps.Element (I), Depth + Depth_Space);
         end Print;
      begin
         if This /= null then
            if This.Is_Root then
               Put_Line (Prefix & "@ - " & Image (+This.Data));
            else
               Put_Line (Prefix &
                         Image (+This.Index) & " - " &
                         Image (+This.Data));
            end if;
            This.Children.Iterate (Print'Access);
         end if;
      end Print;
   begin
      if This.Root /= null then
         Print (This.Root);
      else
         Put_Line ("NULL TREE");
      end if;
   end Print;

   procedure Print_Debug is new Print;

   -----------
   -- Clone --
   -----------

   procedure Clone (Pos : Cursor'Class;
                    Src : Node_Access)
   is
      pragma Assert (Pos.Ref.Root /= Src.Root);
      procedure Clone_Internal (Pos :              Cursor;
                                Src :     not null Node_Access)
      is
         procedure Clone_Child (I : Node_Maps.Cursor) is
            S_Child : constant Node_Access := Node_Maps.Element (I);
         begin
            Clone_Internal (Pos.Child (+S_Child.Index), S_Child);
         end Clone_Child;
      begin
         Pos.Insert (+Src.Data);
--           Agpl.Text_Io.Put_Line ("Pre-child");
--           Print_Debug (Tree (Pos.Root.all));
         Src.Children.Iterate (Clone_Child'Access);
--           Agpl.Text_Io.Put_Line ("Post-child");
--           Print_Debug (Tree (Pos.Root.all));
      end Clone_Internal;
   begin
      if Src = null then
         raise Constraint_Error with "Cannot clone null source";
      end if;

      --  Clear branch being replaced by the clone, if existing:
      Pos.Clear;

      --  And start cloning
      Clone_Internal (Cursor (Pos), Src);
   end Clone;

   -----------
   -- Merge --
   -----------

   procedure Merge
     (Dst : in out Tree;
      Src :        Tree)
   is
      use Agpl.Text_Io;
      procedure Merge (Pos :             Cursor;
                       Src_Node :        Node_Access)
      is
         procedure Merge_Child (I : Node_Maps.Cursor) is
            Index     : constant Child_Index := Node_Maps.Key (I);
         begin
            Merge (Pos.Child (Index),
                   Node_Maps.Element (I));
         end Merge_Child;
      begin
         if Pos.Ref.Current /= null then
            Replace_Data (Pos.Ref.Current.all,
                          Merge_Node (+Pos.Ref.Current.Data, +Src_Node.Data));
            Src_Node.Children.Iterate (Merge_Child'Access);
         else
            Clone (Pos, Src_Node);
         end if;
      end Merge;
   begin
      if Src.Root /= null then
         Merge (Cursor (Dst.Get_Root), Src.Root);
      end if;
   end Merge;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Tree)
   is
   begin
      Clear (This.Root);
   end Finalize;

   -----------------
   -- Make_Cursor --
   -----------------

   function Make_Cursor (This : not null Node_Access) return Cursor is
   begin
      if This.Is_Root then
         return
           Bind (new Precursor'(Is_Root => True,
                                Root    => This.Root,
                                Parent  => This.Parent,
                                Current => This));
      else
         return
           Bind (new Precursor'(Is_Root => False,
                                Root    => This.Root,
                                Parent  => This.Parent,
                                Current => This,
                                Index   => This.Index));
      end if;
   end Make_Cursor;

   ------------
   -- Insert --
   ------------

   procedure Insert (This : Cursor;
                     Src  : Tree'Class)
   is
   begin
      if This.Ref.Root = Src.This then
         raise Constraint_Error with "Cannot copy tree into itself";
      end if;
      if This.Ref.Current /= null then
         raise Constraint_Error with "Node already exists in Insert";
      end if;
      Clone (This, Src.Root);
      if This.Ref.Parent = null then
         This.Ref.Root.Root := This.Ref.Current;
      end if;
   end Insert;

   -------------
   -- Include --
   -------------

   procedure Include (This : Cursor;
                      Src  : Tree'Class)
   is
   begin
      if This.Ref.Root = Src.This then
         raise Constraint_Error with "Cannot copy tree into itself";
      elsif This.Has_Element then
         This.Clear;
      end if;
      This.Insert (Src);
   end Include;

   ----------
   -- Copy --
   ----------

   procedure Copy (Src :        Cursor'Class;
                   Dst : in out Tree)
   is
   begin
      Clone (Dst.Get_Root, Src.Ref.Current);
   end Copy;

end Agpl.Containers.Unbounded_Trees;
