with Ada.Finalization;

generic
   type Node_Data (<>) is private;
   type Child_Index is (<>);
package Agpl.Containers.Ntrees is

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

   function Element (This : Cursor) return Node_Data;

   function Update (This : Cursor) return not null access Node_Data;

   function Query (This : Cursor) return not null access constant Node_Data;

   function Parent (This : Cursor) return Cursor;

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

   procedure Copy (Src :     Cursor'Class;
                   Dst : out Tree);
   --  Make a tree from another node.

   procedure Clear (This : Cursor);
   --  Cleanses from here downwards

   procedure Iterate (This  : Tree;
                      Query : not null access procedure (I : Cursor));

   procedure Iterate_Children
     (This  : Cursor;
      Query : not null access procedure (I : Cursor));

   generic
      with function Merge_Node (X, Y : Node_Data) return Node_Data;
   procedure Merge (Dst : in out Tree;
                    Src :        Tree);

   generic
      with function Image (X : Node_Data) return String;
   procedure Print (This : Tree);
   --  Debug dump, Depth first.

private

   type Tree_Access is access all Tree;
   type Node_Data_Access is access all Node_Data;

   type Node (<>);
   type Node_Access is access all Node;

   type Node_Array is array (Child_Index) of Node_Access;

   type Node (Root : Tree_access; Parent : Node_Access) is new
     Ada.Finalization.Limited_Controlled
   with
      record
         Data     : Node_Data_Access;

         Index    : Child_Index; -- Own position in respect to parent
         Children : Node_Array;
      end record;

   procedure Finalize (This : in out Node);

   function Make_Cursor (This : not null Node_Access) return Cursor;

   function Clone (Dst : Tree;
                   Par : Node_Access; -- parent
                   Idx : Child_Index; -- position will use
                   Src : Node_Access) return Node_Access;
   --  Copy Src branch, making Dst its Tree, Par its parent, Idx its pos

   procedure Replace_Data (This : in out Node;
                           Data :        Node_Data);

   type Cursor is tagged record
      Root                  : Tree_Access;
      Parent, Current       : Node_Access;
      Index                 : Child_Index;
      This  : access Cursor := Cursor'Unrestricted_Access;
   end record;

   type Tree is new Ada.Finalization.Limited_Controlled with record
      This : Tree_Access := Tree'Unchecked_Access;
      Root : Node_Access;
   end record;

   overriding
   procedure Finalize (This : in out Tree);

   Null_Cursor : constant Cursor :=
                   (Root    => null,
                    Parent  => null,
                    Current => null,
                    Index   => Child_Index'First,
                    This    => <>);

end Agpl.Containers.Ntrees;
