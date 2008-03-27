 

--  ADA GENERAL PURPOSE LIBRARY

with Ada.Finalization;

package Agpl.Context_controllers is

   pragma Preelaborate;

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   --  Declaring an object of this kind will cause the execution of the
   --  Beginning and Ending code when the object is created and destroyed
   --  respectively

   type Code_access is access procedure;

   type Simple_controller (Beginning, Ending : Code_access) is private;

private

   use Ada;

   type Simple_controller (Beginning, Ending : Code_access) is new
   Finalization.Controlled with null record;

   procedure Initialize (This : in out Simple_controller);
   procedure Finalize   (This : in out Simple_controller);

end Agpl.Context_controllers;
