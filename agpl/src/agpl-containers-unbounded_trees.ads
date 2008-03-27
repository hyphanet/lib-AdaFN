--  with Agpl.Smart_Access_Limited_Debug;
with Agpl.Smart_Access_Limited;

with Ada.Containers.Indefinite_Ordered_Maps,
     Ada.Finalization,
     Agpl.Generic_Handle;

--  with Gnat.Debug_Pools;

generic
   type Node_Data   (<>) is private;
   type Child_Index (<>) is private;
   with function "<"   (L, R  : Child_Index) return Boolean is <>;
   --  Must be non-modular; it is internally used for sets of children
package Agpl.Containers.Unbounded_Trees is

   pragma Preelaborate;

   type Tree is tagged limited private;
   --  Because I haven't had the time to implement copying, but can be done

   type Cursor (<>) is tagged private;
   --  Used to navigate a tree. Basically a pointer to node.

   function Has_Element (This : Cursor) return Boolean;
   --  If this is false, there are two possibilities:
   --  One, this is a node yet undefined: root, some null child.
   --    In this case, the cursor is usable for insertions at this position.
   --  Two, this may be an invalid "next sibling", which makes no sense.
   --    Thus inserting using such a cursor will fail.

   function Has_Position (This : Cursor) return Boolean;
   --  A cursor may not have element, but still point to a valid position in
   --  the tree (e.g. a to-be-child). This kind of cursor is useable for
   --  insertions, while others without position aren't

   function Index (This : Cursor) return Child_Index;
   --  What it is in respect to parent.
   --  Fails for root

   function Is_Leaf (This : Cursor) return Boolean;

   function Root (This : Tree) return Cursor'Class;

   function First (This : Tree) return Cursor'Class renames Root;

   function Last (This : Tree) return Cursor'Class;

   function Root (This : Cursor) return not null access Tree'Class;

   function Root (This : Cursor) return Cursor;
   --  Gets the tree root of this node.

   function Is_Root (This : Cursor) return Boolean;

   function Element (This : Cursor) return Node_Data;

   function Update (This : Cursor) return not null access Node_Data;

   function Query (This : Cursor) return not null access constant Node_Data;

   function Parent (This : Cursor) return Cursor;

   function Has_Children (This : Cursor) return Boolean;

   function Child (This  : Cursor;
                   Which : Child_Index) return Cursor;

   function First_Child (This : Cursor) return Cursor;

   function Last_Child (This : Cursor) return Cursor;

   function Previous_Sibling (This : Cursor) return Cursor;

   function Next_Sibling (This : Cursor) return Cursor;
   --  These two skip over null nodes.

   procedure Insert (This : Cursor;
                     Data : Node_Data);
   --  This must not have element

   procedure Include (This : Cursor;
                      Data : Node_Data);
   --  This may or not may have element.

   procedure Insert (This : Cursor;
                     Src  : Tree'Class);
   --  Insert Src at This.
   --  Src /= This.tree!

   procedure Include (This : Cursor;
                      Src  : Tree'Class);

   procedure Copy (Src :        Cursor'Class;
                   Dst : in out Tree);
   --  Make a tree from another node.

   procedure Clear (This : Cursor);
   --  Cleanses from here downwards

   procedure Iterate (This  : Tree;
                      Query : not null access procedure (I : Cursor));
   --  Preorder traversal

   procedure Iterate_Children
     (This  : Cursor;
      Query : not null access procedure (I : Cursor));

   generic
      with function Precedes (L, R : Cursor) return Boolean is <>;
   procedure Iterate_Ordered_Children
     (This  : Cursor;
      Query : not null access procedure (I : Cursor));
   --  Visits the children in a new given order.
   --  The cursors passed to Precedes are of children nodes
   --  O (n log n)

   generic
      with function Merge_Node (X, Y : Node_Data) return Node_Data;
   procedure Merge (Dst : in out Tree;
                    Src :        Tree);

   generic
      with function Image (X     : Node_Data)   return String is <>;
      with function Image (Index : Child_Index) return String is <>;
      Depth_Space : Positive := 2;
   procedure Print (This : Tree);
   --  Debug dump, Depth first.

private

   package Data_Handles is new Generic_Handle (Node_Data);
   type Data_Handle is new Data_Handles.Object with null record;

   package Index_Handles is new Generic_Handle (Child_Index);
   type Index_Handle is new Index_Handles.Object with null record;

   type Tree_Access is access all Tree;

--   Pool : Gnat.Debug_Pools.Debug_Pool;

   type Node (<>);
   type Node_Access is access all Node;
--   for Node_Access'Storage_Pool use Pool;

   package Node_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps (Child_Index, Node_Access);

   type Node (Root    : Tree_access;
              Parent  : Node_Access;
              Is_Root : Boolean) is limited
      record
         Data     : Data_Handle;
         Children : Node_Maps.Map;
         case Is_Root is
            when False =>
               Index : Index_Handle; -- Own position in respect to parent
            when True => null;
         end case;
      end record;

   function Make_Cursor (This : not null Node_Access) return Cursor;

   procedure Clone (Pos : Cursor'Class;
                    Src : Node_Access);
   --  Copy Src branch, making Dst its Tree, at Pos

   procedure Replace_Data (This : in out Node;
                           Data :        Node_Data);

   type Precursor (Is_Root : Boolean) is limited record
      Root                  : Tree_Access;
      Parent, Current       : Node_Access;
      case Is_Root is
         when False =>
            Index : Index_Handle;
         when True  => null;
      end case;
   end record;

   type Precursor_Access is access all Precursor;

   package Cursors is
     new Agpl.Smart_Access_Limited (Precursor, Precursor_Access);

   type Cursor is new Cursors.Object with null record;

   type Tree is new Ada.Finalization.Limited_Controlled with record
      This : Tree_Access := Tree'Unchecked_Access;
      Root : Node_Access;
   end record;

   overriding
   procedure Finalize (This : in out Tree);

   function Get_Root (This : Tree) return Cursor'Class renames Root;

end Agpl.Containers.Unbounded_Trees;
