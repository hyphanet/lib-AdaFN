 

--  Just implements sections but still doesn't trace.

with Agpl.Trace.Root;

package Agpl.Trace.Console is

   pragma Preelaborate;

   type Object is new Root.Object with private;
   type Object_Access is access all Object'Class;

   pragma Preelaborable_Initialization (Object);

   overriding
   procedure Log (This    : in out Object;
                  Text    : in String;
                  Level   : in Levels;
                  Section : in String := "");

private

   type Object is new Root.Object with null record;

end Agpl.Trace.Console;
