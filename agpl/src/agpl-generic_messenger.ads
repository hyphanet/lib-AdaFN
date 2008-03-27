 

--  This class allows for a kind of "signaling" between an emitter and several
--  consumers.

--  Since there's no selectable signals for registration, it should not be used
--  for heavy traffic, I suppose.

with Ada.Containers.Indefinite_Doubly_Linked_Lists;

generic
   type Signal_Kind is (<>);
   type Message_Data (<>) is limited private;
package Agpl.Generic_Messenger is

   pragma Preelaborate;

   type Object is abstract tagged null record;

   procedure Signal (This : in out Object;
                     Kind : in     Signal_Kind;
                     Data : in     Message_Data) is abstract;
   --  The emitter calls this function to notify all overriden descendents.
   --  A bidirectional communication must then ensue by means of @This@

   package Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists (Object'Class);

   type Manager is tagged private;

   procedure Add_Listener (This : in out Manager; X : Object'Class);

   procedure Signal (This : in out Manager;
                     Kind : in     Signal_Kind;
                     Data : in     Message_Data);
   --  Will propagate the signal to all its managed objects.

private

   type Manager is tagged record
      Objects : Lists.List;
   end record;

end Agpl.Generic_Messenger;
