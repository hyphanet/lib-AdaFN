 

with Ada.Strings.Unbounded;

package Dom.Core.Nodes.Output is

   package ASU renames Ada.Strings.Unbounded;

   --  Based on functions from Xml/Ada:
   --  Doesn't perform encoding.
   procedure Print
     (N              : Node;
      U              : in out ASU.Unbounded_string;
      Print_Comments : Boolean := False;
      Print_XML_PI   : Boolean := False;
      With_URI       : Boolean := False;
      Indent         : Natural := 0);
   --  Print the contents of Node and its children in XML format.
   --  If Print_Comments is True, then nodes associated with comments are
   --  also displayed.
   --  The <?xml?> processing instruction is displayed only if Print_XML_PI
   --  By default, names are of the form  ns_prefix:local_name. However, if
   --  with_URI is True, names will be  ns_URI:local_name instead

   procedure Print
     (N              : Node;
      File           : String;
      Print_Comments : Boolean := False;
      Print_XML_PI   : Boolean := False;
      With_URI       : Boolean := False;
      Indent         : Natural := 0);
   --  As previous but to file

   procedure Print
     (List           : Dom.Core.Node_List;
      U              : in out ASU.Unbounded_string;
      Print_Comments : Boolean := False;
      Print_XML_PI   : Boolean := False;
      With_URI       : Boolean := False;
      Indent         : Natural := 0);
   --  Same as Print, but for all the nodes in the list.

end Dom.Core.Nodes.Output;
