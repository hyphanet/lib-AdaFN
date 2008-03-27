 

package body Agpl.Smart_Access is

   ---------
   -- Val --
   ---------
   --  Get value
   --  Of course, if the access value is copied outside, error can occur.
   function Val (This : in Object) return Item is
   begin
      return This.Ref.all;
   end Val;

   -----------
   -- Input --
   -----------

   function Input (S : access Ada.Streams.Root_Stream_Type'Class) return Object
   is
      Valid : constant Boolean := Boolean'Input (S);
      This  : Object;
   begin
      if Valid then
         declare
            Data : constant Item_Access := new Item'(Item'Input (S));
         begin
            This.Bind (Data);
         end;
      end if;
      return This;
   end Input;

   ------------
   -- Output --
   ------------

   procedure Output (S    : access Ada.Streams.Root_Stream_Type'Class;
                     This : in Object)
   is
   begin
      Boolean'Output (S, Is_Null (This));
      if not Is_Null (This) then
         Item'Output (S, Val (This));
      end if;
   end Output;

end Agpl.Smart_Access;
