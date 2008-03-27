with Agpl.Text_Io;

package body Agpl.Smart_Access_Limited_Debug is

   -------------
   -- Is_Null --
   -------------

   function Is_Null (This : in Object) return Boolean is
   begin
      return This.Data = null;
   end Is_Null;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (This : in Object) return Boolean is
   begin
      return not This.Is_Null;
   end Is_Valid;

   ----------
   -- Bind --
   ----------

   procedure Bind (This : in out Object; Data : in Item_Access) is
   begin
      --  Text_Io.Put_Line ("At bind (procedure)");
      This.Data := Data;
   end Bind;

   ----------
   -- Bind --
   ----------

   function Bind (This : in     Item_Access) return Object is
   begin
      --  Text_Io.Put_Line ("At bind (function)");
      return Object'(Data => This);
   end Bind;

   ---------
   -- Ref --
   ---------

   function Ref (This : in Object) return Item_Access is
   begin
      return This.Data;
   end Ref;

   ------------
   -- Rebind --
   ------------

   procedure Rebind
     (This  : in out Object;
      Data  : in     Item_Access;
      Force : in     Boolean := False)
   is
   begin
      if This.Data = null or else Force then
         This.Data := Data;
      else
         raise Allocated_Access;
      end if;
   end Rebind;

   ------------
   -- Unbind --
   ------------

   procedure Unbind (This : in out Object; Force : in Boolean := False) is
   begin
      This.Data := null;
   end Unbind;

end Agpl.Smart_Access_Limited_Debug;
