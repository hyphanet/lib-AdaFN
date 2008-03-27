with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Finalization;
with Ada.Strings.Hash;
use  Ada;

package Agpl.Counter.Multi is

   pragma Preelaborate;

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   type Object is tagged limited private;
   pragma Preelaborable_Initialization (Object);

   ------------------------------------------------------------------------
   -- Add                                                                --
   ------------------------------------------------------------------------
   --  Creates it if doesn't exists with value Increment
   procedure Add (This : in out Object; Key : in String; Increment : in Integer := 1);

   ------------------------------------------------------------------------
   -- Reset                                                              --
   ------------------------------------------------------------------------
   procedure Reset (This : in out Object; Key : in String; Val : in Integer := 0);

   ------------------------------------------------------------------------
   -- Val                                                                --
   ------------------------------------------------------------------------
   function  Val (This : in Object; Key : in String) return Integer;

   ------------------------------------------------------------------------
   -- Max_Key                                                            --
   ------------------------------------------------------------------------
   function  Max_Key (This : in Object) return String; -- Key with highest value

private

   package Counter_Map is new Ada.Containers.Indefinite_Hashed_Maps
     (String,
      Counter.Object_Access,
      Ada.Strings.Hash,
      "=",
      "=");

   ------------------------------------------------------------------------
   -- Safe                                                               --
   ------------------------------------------------------------------------
   protected type Safe_Object (Parent : access Object) is
      --  Creates it if doesn't exists with value Increment
      procedure Add (Key : in String; Increment : in Integer := 1);
      procedure Reset (Key : in String; Val     : in Integer := 0);
      function  Val (Key : in String) return Integer;
      function  Max_Key return String; -- Key with highest value
      procedure Destroy;
   private
      Values     : aliased Counter_Map.Map;
   end Safe_Object;

   type Object is new Finalization.Limited_Controlled with record
      Safe : Safe_Object (Object'Access);
   end record;

   procedure Finalize (This : in out Object);

end Agpl.Counter.Multi;
