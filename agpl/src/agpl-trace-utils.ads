 

--  Objects for tracing with disc dump optional. This first implementation uses
--  a protected object but not queuing. Writing is done within the protected,
--  which is theoretically illega. Gnat's implementation of I/O allows it so in
--  this first approach we'll leave it like that.

package Agpl.Trace.Utils is

   pragma Elaborate_Body;

   --  Prependers:

   function Prepend_Level (Text    : in String;
                           Level   : in Levels;
                           Section : in String) return String;
   --  Adds a marker of message level
   pragma Inline (Prepend_Level);

   function Prepend_Timestamp (Text    : in String;
                               Level   : in Levels;
                               Section : in String) return String;
   --  Add a timestamp.
   pragma Inline (Prepend_Timestamp);

   function Prepend_Level_Timestamp (Text    : in String;
                                     Level   : in Levels;
                                     Section : in String) return String;
   --  Add level & timestamp
   pragma Inline (Prepend_Level_Timestamp);

   function Prepend_Level_Timestamp_Section (Text    : in String;
                                             Level   : in Levels;
                                             Section : in String) return String;
   pragma Inline (Prepend_Level_Timestamp_Section);

end Agpl.Trace.Utils;
