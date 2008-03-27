with Agpl.Text_Io;

with Ada.Unchecked_Deallocation;

package body Agpl.Containers.Ntrees is

   procedure Free is new Ada.Unchecked_Deallocation (Node, Node_Access);
   procedure Free is new Ada.Unchecked_Deallocation (Node_Data, Node_Data_Access);

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element (This : Cursor) return Boolean is
   begin
      return This.Current /= null;
   end Has_Element;

   -----------
   -- Index --
   -----------

   function Index (This : Cursor) return Child_Index is
   begin
      return This.Current.Index;
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
        Cursor'Class
          (Cursor'
               (This.This,
                null,
                This.Root,
                Child_Index'First,
                This => <>));
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
      return This.Root;
   end Root;

   ----------
   -- Root --
   ----------

   function Root (This : Cursor) return Cursor is
   begin
      return Cursor (Root (This.Root.all));
   end Root;

   -------------
   -- Element --
   -------------

   function Element (This : Cursor) return Node_Data is
   begin
      return This.Current.Data.all;
   end Element;

   ------------
   -- Update --
   ------------

   function Update (This : Cursor) return not null access Node_Data is
   begin
      return This.Current.Data;
   end Update;

   -----------
   -- Query --
   -----------

   function Query (This : Cursor) return not null access constant Node_Data is
   begin
      return This.Current.Data;
   end Query;

   ------------
   -- Parent --
   ------------

   function Parent (This : Cursor) return Cursor is
   begin
      return (This.Root,
              This.Parent.Parent,
              This.Parent,
              This.Parent.Index,
              This => <>);
   end Parent;

   -----------
   -- Child --
   -----------

   function Child
     (This  : Cursor;
      Which : Child_Index)
      return Cursor
   is
   begin
      return (This.Root,
              This.Current,
              This.Current.Children (Which),
              Which,
              This => <>);
   end Child;

   -----------------
   -- First_Child --
   -----------------

   function First_Child (This : Cursor) return Cursor is
   begin
      for I in This.Current.Children'Range loop
         if This.Current.Children (I) /= null then
            return (This.Root,
                    This.Current,
                    This.Current.Children (I),
                    I,
                    This => <>);
         end if;
      end loop;

      return (This.Root,
              This.Current,
              null,
              Child_Index'First,
              This => <>);
   end First_Child;

   ----------------
   -- Last_Child --
   ----------------

   function Last_Child (This : Cursor) return Cursor is
   begin
      for I in reverse This.Current.Children'Range loop
         if This.Current.Children (I) /= null then
            return (This.Root,
                    This.Current,
                    This.Current.Children (I),
                    I,
                    This => <>);
         end if;
      end loop;

      return (This.Root,
              This.Current,
              null,
              Child_Index'Last,
              This => <>);
   end Last_Child;

   ----------------------
   -- Previous_Sibling --
   ----------------------

   function Previous_Sibling (This : Cursor) return Cursor is
   begin
      if This.Index = Child_Index'First then
         return Null_Cursor;
      else
         for I in reverse Child_Index'Pred (This.Index) .. Child_Index'First
         loop
            if This.Parent.Children (I) /= null then
               return (This.Root,
                       This.Parent,
                       This.Parent.Children (I),
                       I,
                       This => <>);
            end if;
         end loop;

         return (This.Root,
                 This.Parent,
                 null,
                 Child_Index'Pred (This.Index),
                 This => <>);
      end if;
   end Previous_Sibling;

   ------------------
   -- Next_Sibling --
   ------------------

   function Next_Sibling (This : Cursor) return Cursor is
   begin
      if This.Index = Child_Index'Last then
         return Null_Cursor;
      else
         for I in Child_Index'Succ (This.Index) .. Child_Index'Last
         loop
            if This.Parent.Children (I) /= null then
               return (This.Root,
                       This.Parent,
                       This.Parent.Children (I),
                       I,
                       This => <>);
            end if;
         end loop;

         return (This.Root,
                 This.Parent,
                 null,
                 Child_Index'Succ (This.Index),
                 This => <>);
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
      if This.Current /= null then
         raise Constraint_Error with "Node already exists in Insert";
      else
         This.This.Current := new Node'(Ada.Finalization.Limited_Controlled with
                                        Root   => This.Root,
                                        Parent => This.Parent,
                                        Data   => new Node_Data'(Data),
                                        Index  => This.Index,
                                        Children => <>);

         if This.Parent = null then
            This.Root.Root := This.This.Current;
         else
            This.Parent.Children (This.Index) := This.This.Current;
         end if;
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
      if This.Current /= null then
         Replace_Data (This.This.Current.all, Data);
      else
         This.Insert (Data);
      end if;
   end Include;

   procedure Replace_Data (This : in out Node;
                           Data :        Node_Data)
   is
   begin
      Free (This.Data);
      This.Data := new Node_Data'(Data);
   end Replace_Data;

   -----------
   -- Clear --
   -----------

   procedure Clear (This : in out Node_Access) is
   begin
      if This /= null then
         for I in This.Children'Range loop
            Clear (This.Children (I));
         end loop;
         Free (This);
      end if;
   end Clear;

   procedure Clear (This : Cursor) is
   begin
      Clear (This.This.Current);
   end Clear;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (This  : Tree;
      Query : not null access procedure (I : Cursor))
   is
      procedure Iterate (This : Node_Access) is
      begin
         if This /= null then
            Query (Make_Cursor (This));
            for I in This.Children'Range loop
               Iterate (This.Children (I));
            end loop;
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
   begin
      for I in This.Current.Children'Range loop
         if This.Current.Children (I) /= null then
            Query (Make_Cursor (This.Current.Children (I)));
         end if;
      end loop;
   end Iterate_Children;

   -----------
   -- Clone --
   -----------

   function Clone (Dst : Tree;
                   Par : Node_Access;
                   Idx : Child_Index;
                   Src : Node_Access) return Node_Access
   is
      Ptr : Node_Access := Src;
      procedure Clone (D : in out Node_Access;
                       P :        Node_Access;
                       S :        Node_Access) is
      begin
         if S /= null then
            D := new Node'(Ada.Finalization.Limited_Controlled with
                           Root   => Dst.This,
                           Parent => P,
                           Data   => new Node_Data'(S.Data.all),
                           Index  => S.Index,
                           Children => S.Children);
            for I in Child_Index'Range loop
               Clone (D.Children (I), D, S.Children (I));
            end loop;
         else
            D := null;
         end if;
      end Clone;
   begin
      Clone (Ptr, Par, Src);
      Ptr.Index := Idx; -- Force position that may have changed
      if Par /= null then
         Par.Children (Idx) := Ptr;
      end if;
      return Ptr;
   end Clone;

   -----------
   -- Merge --
   -----------

   procedure Merge
     (Dst : in out Tree;
      Src :        Tree)
   is
      procedure Merge (D : in out Node_Access;
                       P : Node_Access;
                       S : Node_Access) is
      begin
         if S /= null then
            if D /= null then
               Replace_Data (D.all, Merge_Node (D.Data.all, S.Data.all));
               for I in Child_Index'Range loop
                  Merge (D.Children (I), D, S.Children (I));
               end loop;
            else
               D := Clone (Dst, P, S.Index, S);
            end if;
         end if;
      end Merge;
   begin
      Merge (Dst.Root, null, Src.Root);
   end Merge;

   -----------
   -- Print --
   -----------

   procedure Print (This : Tree) is
      procedure Print (This : Node_Access; Depth : Natural := 0) is
         use Agpl.Text_Io;
         function Prefix return String is
         begin
            return String'(1 .. Depth => '-');
         end Prefix;
      begin
         if This /= null then
            Put_Line (Prefix & This.Index'Img & " - " & Image (This.Data.all));
            for I in This.Children'Range loop
               Print (This.Children (I), Depth + 3);
            end loop;
         end if;
      end Print;
   begin
      Print (This.Root);
   end Print;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Node) is
   begin
      Free (This.Data);
   end Finalize;

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
      return (Root    => This.Root,
              Parent  => This.Parent,
              Current => This,
              Index   => This.Index,
              This    => <>);
   end Make_Cursor;

   ------------
   -- Insert --
   ------------

   procedure Insert (This : Cursor;
                     Src  : Tree'Class)
   is
   begin
      if This.Root = Src.This then
         raise Constraint_Error with "Cannot copy tree into itself";
      end if;
      if This.Current /= null then
         raise Constraint_Error with "Node already exists in Insert";
      end if;
      This.This.Current := Clone (This.Root.all,
                                  This.Parent,
                                  This.Index,
                                  Src.Root);
      if This.Parent = null then
         This.Root.Root := This.This.Current;
      end if;
   end Insert;

   -------------
   -- Include --
   -------------

   procedure Include (This : Cursor;
                      Src  : Tree'Class)
   is
   begin
      if This.Root = Src.This then
         raise Constraint_Error with "Cannot copy tree into itself";
      elsif This.Has_Element then
         This.Clear;
      end if;
      This.Insert (Src);
   end Include;

   ----------
   -- Copy --
   ----------

   procedure Copy (Src :     Cursor'Class;
                   Dst : out Tree)
   is
   begin
      Dst.Root := Clone (Dst.This.all, null, Child_Index'First, Src.Current);
   end Copy;

end Agpl.Containers.Ntrees;
