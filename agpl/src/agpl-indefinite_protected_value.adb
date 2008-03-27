 

with Ada.Unchecked_Deallocation;

package body Agpl.Indefinite_Protected_Value is

   procedure Free is
     new Ada.Unchecked_Deallocation (Element_Type, Element_Access);

   -----------
   -- Empty --
   -----------

   function Empty (This : in Object) return Boolean is
   begin
      return This.Internal.Empty;
   end Empty;

   ---------
   -- Get --
   ---------

   function Get (This : in Object) return Element_Type is
   begin
      return This.Internal.Get;
   end Get;

   ---------
   -- Set --
   ---------

   procedure Set (This : in out Object; That : Element_Type) is
   begin
      This.Internal.Set (That);
   end Set;

   -------------
   -- Operate --
   -------------

   procedure Operate (This : in out Object; Using : in out Functor'Class) is
   begin
      This.Internal.Operate (Using);
   end Operate;

   ---------------------
   -- Internal_Object --
   ---------------------

   protected body Internal_Object is

      -----------
      -- Empty --
      -----------

      function Empty return Boolean is
      begin
         return Value = null;
      end Empty;

      ---------
      -- Get --
      ---------

      function Get return Element_Type is
      begin
         return Value.all;
      end Get;

      ---------
      -- Set --
      ---------

      procedure Set (This  : in Element_Type) is
      begin
         Free (Value);
         Value := new Element_Type'(This);
      end Set;

      -------------
      -- Operate --
      -------------

      procedure Operate (Using : in out Functor'Class) is
      begin
         Operate (Using, Value.all);
      end Operate;

      ----------
      -- Free --
      ----------

      procedure Free is
      begin
         Free (Value);
      end Free;

   end Internal_Object;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Object) is
   begin
      This.Internal.Free;
   end Finalize;

end Agpl.Indefinite_Protected_Value;
