 

--  Just implements sections but still doesn't trace.

with Agpl.Containers.String_Sets;

package Agpl.Trace.Root is

   pragma Preelaborate;

   type Object is limited new Trace.Object with private;
   type Object_Access is access all Object'Class;

   pragma Preelaborable_Initialization (Object);

   overriding
   procedure Log (This    : in out Object;
                  Text    : in     String;
                  Level   : in     Levels;
                  Section : in     String := "") is null;
   --  No need for implementations of this object to call Must_Log, it's called
   --  in the Agpl.Trace.Log subprogram.

   overriding
   function Must_Log (This    : in Object;
                      Level   : in Levels;
                      Section : in String) return Boolean;

   overriding
   procedure Enable_Section  (This    : in out Object;
                              Section : in     String;
                              Enabled : in     Boolean := True);

   overriding
   procedure Set_Active (This : in out Object; Active : in Boolean := True);

   overriding
   procedure Set_Level  (This : in out Object; Level : in All_Levels);

   overriding
   procedure Set_Decorator (This : in out Object; Decor : in Decorator);

   overriding
   function Decorate (This    : in Object;
                      Text    : in String;
                      Level   : in Levels;
                      Section : in String) return String;

private

   type Object is limited new Trace.Object with record
      Active   : Boolean    := True;
      Level    : All_Levels := Informative;
      Sections : Containers.String_Sets.Set;
      Decor    : Decorator;
   end record;

end Agpl.Trace.Root;
