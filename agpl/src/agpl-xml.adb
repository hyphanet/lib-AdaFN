

--  Xml related functions

with Agpl.Strings;
with Agpl.Strings.Fields; use Agpl.Strings.Fields;
--  with Agpl.Trace; use Agpl.Trace;
with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with DOM.Core.Documents;
with DOM.Core.Elements;
with Dom.Core.Nodes.Output;
with DOM.Readers;
with Input_Sources.File;
with Input_Sources.Strings;
with Sax.Readers;

with Ada.Exceptions; use Ada.Exceptions;
with Text_IO;

package body Agpl.Xml is

   package DCD renames DOM.Core.Documents;
   package DCE renames DOM.Core.Elements;
   package DCN renames DOM.Core.Nodes;

   use type DOM.Core.Node;
   use type DOM.Core.Node_Types;

   Unsafe_Chars : constant array (Character) of Boolean :=
      ('&' | ''' | '"' | '<' | '>' => True,
      others => False);

   Replacings : constant array (Character) of access String :=
      ('&'   => new String'("&amp;"),
      '''    => new String'("&apos;"),
      '"'    => new String'("&quot;"),
      '<'    => new String'("&lt;"),
      '>'    => new String'("&gt;"),
      others => new String'(""));

   function L (this : in String) return String renames Agpl.Strings.To_lower;

   ------------------------------------------------------------------------
   -- From_string                                                        --
   ------------------------------------------------------------------------
   --  Parses and XML string (Latin1 accepted)
   function From_String
     (Data     : in String;
      Encoding : in Unicode.Ces.Encoding_Scheme := Unicode.Ces.Utf8.Utf8_Encoding)
      return Document is
      Tree          : DOM.Readers.Tree_Reader;
      String_handle : Input_Sources.Strings.String_Input;
      N             : Node;
   begin
      --  Needed for namespaces:
      Sax.Readers.Set_Feature
        (Sax.Readers.Reader (Tree),
         Sax.Readers.Namespace_Prefixes_Feature,
         True);
      Sax.Readers.Set_Feature
        (Sax.Readers.Reader (Tree),
         Sax.Readers.Validation_Feature,
         False);
      Sax.Readers.Set_Feature
        (Sax.Readers.Reader (Tree),
         Sax.Readers.External_General_Entities_Feature,
         False);
      Sax.Readers.Set_Feature
        (Sax.Readers.Reader (Tree),
         Sax.Readers.External_Parameter_Entities_Feature,
         False);

      Input_Sources.Strings.Open
        (Data'Unrestricted_Access,
         Encoding,
         String_handle);
      DOM.Readers.Parse (Tree, String_handle);
      Input_Sources.Strings.Close (String_handle);
      N := DCD.Get_Element (DOM.Readers.Get_Tree (Tree));
      DCN.Normalize (N);
      return N;
   end From_String;

   ------------------------------------------------------------------------
   -- Parse                                                              --
   ------------------------------------------------------------------------
   --  Read a XML file and stores it in memory;
   function Parse
     (File     : String)
      return     Document
   is
      Tree        : DOM.Readers.Tree_Reader;
      File_handle : Input_Sources.File.File_Input;
      N           : Node;
   begin
      --  Needed for namespaces:
      Sax.Readers.Set_Feature
        (Sax.Readers.Reader (Tree),
         Sax.Readers.Namespace_Prefixes_Feature,
         True);

      Input_Sources.File.Open (File, File_handle);
--        Input_Sources.File.Set_Encoding
--          (File_handle,
--           Encoding);
      DOM.Readers.Parse (Tree, File_handle);
      Input_Sources.File.Close (File_handle);
      N := DCD.Get_Element (DOM.Readers.Get_Tree (Tree));
      DCN.Normalize (N);
      return N;
   exception
      when E : others =>
         Text_IO.Put_Line ("Syntax error in XML file: " & File);
         Text_IO.Put_Line (Exception_Information (E));
         raise;
   end Parse;

   ---------
   -- Get --
   ---------
   function Get (Parent : Node; Name : String) return Node
   is
   begin
      if Name = "" then
         raise Constraint_Error with "Missing child name";
      end if;

      if Parent.Node_Type /= DOM.Core.Element_Node then
         raise Constraint_Error with "Parent is not an element";
      end if;

      declare
         Nodes : constant Node_Vector := Get_all (Parent, Name);
      begin
         if Natural (Nodes.Length) > 1 then
            raise Constraint_Error;
         elsif Natural (Nodes.Length) = 0 then
            raise Constraint_Error with "Child not present: " & Name;
         else
            return Nodes.First_Element;
         end if;
      end;
   end Get;

   -----------------
   -- Get_Or_Null --
   -----------------

   function Get_Or_Null (Parent : Node; Name : String) return Node is
   begin
      return Get (Parent, Name);
   exception
      when Constraint_Error =>
         return null;
   end Get_Or_Null;

   -------------
   -- Get_All --
   -------------

   function Get_All (Parent : Node; Name : String := "*") return Node_Array is
      V : constant Node_Vector := Get_All (Parent, Name);
      R :          Node_Array (1 .. Natural (V.Length));
   begin
      for I in V.First_Index .. V.Last_Index loop
         R (I) := V.Element (I);
      end loop;
      return R;
   end Get_All;

   -------------
   -- Get_All --
   -------------

   function Get_All (Path : String; Parent : Node) return Node_Array is
      V : constant Node_Vector := Get_All (Path, Parent);
      R :          Node_Array (1 .. Natural (V.Length));
   begin
      for I in V.First_Index .. V.Last_Index loop
         R (I) := V.Element (I);
      end loop;
      return R;
   end Get_All;

   -------------
   -- Get_All --
   -------------
   --  Returns childrens with given name (first is 1):
   --  * means any name.
   function Get_all (Parent : Node; Name : String := "*") return Node_Vector is
   begin
      if Parent = Null_node then
         raise Constraint_Error with "Parent is null";
      end if;

      --  let's create the vector and return it:
      declare
         Children : constant Dom.Core.Node_List := Dcn.Child_Nodes (Parent);
         Result   : Node_Vector;
         Item     : Node;
      begin
         for I in  0 .. DCN.Length (Children) - 1 loop
            Item := DCN.Item (Children, I);
            if Item.Node_Type = DOM.Core.Element_Node
              and then (Name = "*"
                        or else L (DCN.Node_Name (Item)) = L (Name))
            then
               Result.Append (Item);
            end if;
         end loop;
         return Result;
      end;
   end Get_All;

   -------------
   -- Get_all --
   -------------

   function Get_all (Path : String; Parent : Node) return Node_Vector is
   begin
      if Parent = Null_node then
         raise Constraint_Error with "Parent is null";
      elsif Path = "" then
         raise Constraint_Error with "Path is empty";
      end if;

      declare
         use type Asu.Unbounded_String;
         Head : Ustring := +String_Head (Path);
         Tail : Ustring := +String_Tail (Path);
         Curr : Node    := Parent;
         Parent : Integer; pragma Unreferenced (Parent); -- Hide it
      begin
         loop
            if Tail = Null_Ustring then
               return Get_All (Curr, +Head);
            else
               Curr := Get (Curr, +Head);
               Head := +String_Head (+Tail);
               Tail := +String_Tail (+Tail);
            end if;
         end loop;
      end;
   end Get_all;

   ------------------------------------------------------------------------
   -- Get_Attribute                                                      --
   ------------------------------------------------------------------------
   --  Read an attribute from a node:
   function Get_attribute
     (Item          : Node;
      Attr          : String;
      Default : String := "")
      return          String
   is
   begin
      declare
         Result : constant String := DCE.Get_Attribute (Item, Attr);
      begin
         if Result /= "" then
            return Result;
         else
            return Default;
         end if;
      end;
   exception
      when others =>
         if Default /= "" then
            return Default;
         else
            raise Data_Error;
         end if;
   end Get_attribute;

   function Get_numeric_attribute_from_node
     (Item          : Node;
      Attr          : String;
      Default : Number)
      return          Number
   is
   begin
      return Number'Value
               (Get_attribute (Item, Attr, Number'Image (Default)));
   exception
      when others =>
         return Default;
   end Get_numeric_attribute_from_node;

   ------------------------------------------------------------------------
   -- Add                                                                --
   ------------------------------------------------------------------------
   --  Insertion functions:
   --  They return the inserted node.
   function Add (Parent : Node; Name : String) return Node is
   begin
      return DCN.Append_Child
               (Parent,
                DCD.Create_Element (DCN.Owner_Document (Parent), Name));
   end Add;

   --  Add child node (must be for the same doc)
   procedure Add (Parent, Child : in Node) is
      Dummy : constant Node := DCN.Append_Child (Parent, Child);
      pragma Unreferenced (Dummy);
   begin
      null;
   end Add;

   ------------------------------------------------------------------------
   -- Create_Child                                                       --
   ------------------------------------------------------------------------
   --  Creates a node for a document, without inserting it:
   function Create_Child (Parent : in Node; Name : in String) return Node is
   begin
      return DCD.Create_Element (DCN.Owner_Document (Parent), Name);
   end Create_Child;

   -------------------
   -- Set_Attribute --
   -------------------

   procedure Set_Attribute (Item : Node;
                            Attr : String;
                            Val  : String)
   is
   begin
      Dce.Set_Attribute (Item, Attr, Val);
   end Set_Attribute;

   ------------------------------------------------------------------------
   -- Delete                                                             --
   ------------------------------------------------------------------------
   procedure Delete (Item : in out Node) is
      Dummy : Node;
   begin
      if Item /= null then
         --  If it's the root element, we free everything:
         if Dcd.Get_Element (Dcn.Owner_Document (Item)) = Item then
            Dummy := Dcn.Owner_Document (Item);
         else
            Dummy := Dcn.Remove_Child (Dcn.Parent_Node (Item), Item);
         end if;
         Dcn.Free (Dummy, Deep => True);
         Item := null;
      end if;
   end Delete;

   ------------------------------------------------------------------------
   -- Length                                                             --
   ------------------------------------------------------------------------
   --  This function returns the number of nodes found at the given level
   function Length (Path : String; Parent : Node) return Natural is
   begin
      if Path = "" then
         return 1;
      else
         declare
            Nodes : constant Node_array := Get_all (Path, Parent);
         begin
            return Nodes'Length;
         exception
            when Constraint_Error =>
               return 0;
         end;
      end if;
   end Length;

   function Length (Parent : Node; Name : String := "*") return Natural is
   begin
      if Name = "" then
         raise Constraint_Error;
      end if;
      declare
         Nodes : constant Node_array := Get_all (Parent, Name);
      begin
         return Nodes'Length;
      exception
         when Constraint_Error =>
            return 0;
      end;
   end Length;

   ------------------------------------------------------------------------
   -- Get_Value                                                          --
   ------------------------------------------------------------------------
   function Get_value (Item : Node; Default : String := "") return String
   is
      Nodes : constant DOM.Core.Node_List := DCN.Child_Nodes (Item);
   begin
      if DCN.Length (Nodes) = 0 then
         if Default = "" then
            raise Data_Error;
         else
            return Default;
         end if;
      end if;
      for n in  0 .. DCN.Length (Nodes) - 1 loop
         if DCN.Item (Nodes, n).Node_Type = DOM.Core.Text_Node then
            return DCN.Node_Value (DCN.Item (Nodes, n));
         end if;
      end loop;
      if Default /= "" then
         return Default;
      else
         raise Data_Error;
      end if;
   end Get_value;

   ------------------------------------------------------------------------
   -- Escape                                                             --
   ------------------------------------------------------------------------
   --  Takes a Latin1 string and encodes all invalid characters as &, <, etc.
   function Escape (This : in String) return String is
      Aux : UString  := U (This);
      Pos : Positive := 1;
      Cur : Character;
   begin
      loop
         exit when Pos > ASU.Length (Aux);

         Cur := ASU.Element (Aux, Pos);
         if Unsafe_Chars (Cur) then
            ASU.Replace_Slice (Aux, Pos, Pos, Replacings (Cur).all);
         end if;

         Pos := Pos + 1;
      end loop;

      return S (Aux);
   end Escape;

   ------------------------------------------------------------------------
   -- Unescape                                                           --
   ------------------------------------------------------------------------
   --  Restores an escaped string to its original form.
   function Unescape (This : in String) return String is
      Aux : UString := U (This);
      Pos : Natural;
   begin
      for I in  Unsafe_Chars'Range loop
         if Unsafe_Chars (I) then
            loop
               Pos := ASU.Index (Aux, Replacings (I).all);
               exit when Pos < 1;
               ASU.Replace_Slice
                 (Aux,
                  Pos,
                  Pos + Replacings (I)'Length - 1,
                  "" & I);
            end loop;
         end if;
      end loop;

      return S (Aux);
   end Unescape;

   ---------------------------
   -- Get_Generic_Attribute --
   ---------------------------

   function Get_Generic_Attribute
     (Item    : Node;
      Attr    : String;
      Default : Xt)
      return    Xt
   is
   begin
      return Value (Get_Attribute (Item, Attr, Image (Default)));
   end Get_Generic_Attribute;

   -----------------------
   -- Get_Generic_Value --
   -----------------------

   function Get_Generic_Value
     (E       : Elem;
      K       : Key;
      Default : Val)
      return    Val
   is
      function Get is new Get_Generic_Attribute (Val, Image, Value);
   begin
      return Get (Enode (E), Attr (K), Default);
   end Get_Generic_Value;

   ---------------
   -- To_String --
   ---------------

   function To_String (This : Node) return String is
      Us : Ustring;
   begin
      Dom.Core.Nodes.Output.Print (This, Us);
      return +Us;
   end To_String;

end Agpl.Xml;
