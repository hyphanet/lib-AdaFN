--  Xml related functions

with DOM.Core;
with Dom.Core.Nodes;
with Unicode.Ces;
with Unicode.Ces.Utf8;

with Ada.Containers.Vectors;

package Agpl.Xml is

   pragma Elaborate_Body;

   subtype Node is Dom.Core.Node;

   function "=" (L, R : Node) return Boolean renames Dom.Core."=";

   package Node_Vectors is new
     Ada.Containers.Vectors (Positive, Node, Dom.Core."=");

   subtype Document is Node; --  NOT the document but the ROOT element.
   type Node_access is access all Node;
   type Document_access is access all Document;
   type Node_Array is array (Integer range <>) of Node;
   subtype Node_Vector is Node_Vectors.Vector;

   Null_node : constant Node := null;

   Data_Error : exception;

   ------------------------------------------------------------------------
   -- From_String                                                        --
   ------------------------------------------------------------------------
   --  Parses and XML string (Latin1 accepted)
   function From_String
     (Data     : in String;
      Encoding : in Unicode.Ces.Encoding_Scheme := Unicode.Ces.Utf8.Utf8_Encoding)
      return        Document;

   ------------------------------------------------------------------------
   -- Parse                                                              --
   ------------------------------------------------------------------------
   --  Read a XML file and stores it in memory;
   function Parse (File     : String) return     Document;

   ------------------------------------------------------------------------
   -- Get                                                                --
   ------------------------------------------------------------------------
   --  Retrieve the named child. If it doesn exist, exception raised.
   function Get         (Parent : Node; Name : String) return Node;
   function Get_Or_Null (Parent : Node; Name : String) return Node;
   --  Will return null instead of raising exception

   ------------------------------------------------------------------------
   -- Get_All                                                            --
   ------------------------------------------------------------------------
   --  Returns childrens with given name (first is 1): * means any name.
   function Get_all (Parent : Node;   Name   : String := "*") return Node_array;
   function Get_All (Path   : String; Parent : Node) return Node_Array;

   function Get_all (Parent : Node;   Name   : String := "*") return Node_Vector;
   function Get_all (Path   : String; Parent : Node) return Node_Vector;

   ------------------------------------------------------------------------
   -- Get_Attribute                                                      --
   ------------------------------------------------------------------------
   --  Read an attribute from a node: If not present and Default = "",
   --  Data_Error will be raised
   function Get_attribute
     (Item    : Node;
      Attr    : String;
      Default : String := "")
      return    String;

   generic
      type Number is range <>;
   function Get_Numeric_Attribute_From_Node
     (Item    : Node;
      Attr    : String;
      Default : Number)
      return          Number;

   generic
      type Xt (<>) is private;
      with function Image (X : Xt) return String is <>;
      with function Value (S : String) return Xt is <>;
   function Get_Generic_Attribute
     (Item    : Node;
      Attr    : String;
      Default : Xt)
      return    Xt;

   generic
      type Elem (<>) is limited private;
      type Key  (<>) is limited private;
      type Val  (<>) is private;
      with function Enode  (E : Elem) return Node is <>;
      with function Attr   (K : Key)  return String is <>;
      with function Image (X : Val) return String is <>;
      with function Value (S : String) return Val is <>;
   function Get_Generic_Value
     (E       : Elem;
      K       : Key;
      Default : Val)
      return    Val;

   procedure Set_Attribute (Item : Node;
                            Attr : String;
                            Val  : String);

   --  Add a child by giving its name
   function Add (Parent : Node; Name : String) return Node;

   --  Add child node (must be for the same doc)
   procedure Add (Parent, Child : in Node);

   ------------------------------------------------------------------------
   -- Create_Child                                                       --
   ------------------------------------------------------------------------
   --  Creates a node for a document, without inserting it:
   function Create_Child (Parent : in Node; Name : in String) return Node;

   ------------------------------------------------------------------------
   -- Delete                                                             --
   ------------------------------------------------------------------------
   --  Deletion:
   --  Removes the Node from its location in a doc, and frees all memory
   --  Children are also removed.
   procedure Delete (Item : in out Node);

   ------------------------------------------------------------------------
   -- Get_Name                                                           --
   ------------------------------------------------------------------------
   function Get_name (Item : Node) return String renames
     DOM.Core.Nodes.Node_Name;

   ------------------------------------------------------------------------
   -- Get_Value                                                          --
   ------------------------------------------------------------------------
   --  If no value and Default = "", Data_Error will be raised
   function Get_value
     (Item          : Node;
      Default : String := "")
      return          String;

   ------------------------------------------------------------------------
   -- Length                                                             --
   ------------------------------------------------------------------------
   --  This function returns the number of nodes found at the given level If
   --  some intermediate floor is multiple, then exception is raised.
   function Length (Path : String; Parent : Node) return Natural;

   ------------------------------------------------------------------------
   -- Length                                                             --
   ------------------------------------------------------------------------
   --  Returns the number of childs of a node with given name. * means any
   --  name.
   function Length (Parent : Node; Name : String := "*") return Natural;

   ------------------------------------------------------------------------
   -- Escape                                                             --
   ------------------------------------------------------------------------
   --  Takes a Latin1 string and encodes all invalid characters as &, <, etc.
   function Escape (This : in String) return String;

   ------------------------------------------------------------------------
   -- Unescape                                                           --
   ------------------------------------------------------------------------
   --  Restores an escaped string to its original form.
   function Unescape (This : in String) return String;

   ---------------
   -- To_String --
   ---------------

   function To_String (This : Node) return String;
   --  Printable version of the underlying doc.

end Agpl.Xml;
