 

--  Protected container for values of any private type.

with Ada.Finalization;

generic
   type Element_type (<>) is private;
package Agpl.Indefinite_Protected_Value is

   pragma Preelaborate;

   type Functor is abstract tagged null record;
   procedure Operate (This : in out Functor; Value : in out Element_Type)
   is abstract;

   type Object is limited private;

   type Object_Access is access Object;

   function Empty (This : in Object) return Boolean;
   --  Says if no value has been still stored.

   function Get (This : in Object) return Element_Type;

   procedure Set (This : in out Object; That : Element_Type);

   procedure Operate (This : in out Object; Using : in out Functor'Class);

private

   type Element_Access is access all Element_Type;

   protected type Internal_Object is
      function  Empty return Boolean;
      function  Get return Element_Type;
      procedure Set     (This  : in Element_Type);
      procedure Operate (Using : in out Functor'Class);
      --  Will call Functor.Operate (Value)
      procedure Free;
   private
      Value : Element_Access;
   end Internal_Object;

   type Object is new Ada.Finalization.Limited_Controlled with record
      Internal : Internal_Object;
   end record;

   procedure Finalize (This : in out Object);

end Agpl.Indefinite_Protected_Value;
