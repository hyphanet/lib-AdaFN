package body Agpl.Strings.Pointers is

   use Ada.Finalization;

   ------------
   -- Create --
   ------------

   function Create (This : String) return Object is
   begin
      return (Controlled with Ptr => new String'(This));
   end Create;

   ------------
   -- Create --
   ------------

   function Create (This : Ustring) return Object is
   begin
      return (Controlled with Ptr => new String'(+This));
   end Create;

   ---------
   -- Ref --
   ---------

   function Ref (This : Object) return String_Access is
   begin
      return This.Ptr;
   end Ref;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (This : in out Object) is
   begin
      This.Ptr := new String'(This.Ptr.all);
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Object) is
   begin
      Free (This.Ptr);
   end Finalize;

end Agpl.Strings.Pointers;
