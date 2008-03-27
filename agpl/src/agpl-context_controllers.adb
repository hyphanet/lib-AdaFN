 

--  ADA GENERAL PURPOSE LIBRARY

package body Agpl.Context_controllers is

   procedure Initialize (This : in out Simple_controller) is
   begin
      This.Beginning.all;
   end Initialize;

   procedure Finalize   (This : in out Simple_controller) is
   begin
      This.Ending.all;
   end Finalize;

end Agpl.Context_controllers;
