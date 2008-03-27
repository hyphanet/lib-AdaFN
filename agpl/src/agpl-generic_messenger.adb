package body Agpl.Generic_Messenger is

   ------------------
   -- Add_Listener --
   ------------------

   procedure Add_Listener (This : in out Manager; X : Object'Class) is
   begin
      This.Objects.Append (X);
   end Add_Listener;

   ------------
   -- Signal --
   ------------

   procedure Signal
     (This : in out Manager;
      Kind : in     Signal_Kind;
      Data : in     Message_Data)
   is

      ----------
      -- Call --
      ----------

      procedure Call (X : in out Object'Class) is
      begin
         Signal (X, Kind, Data);
      end Call;

      procedure Iter (I : Lists.Cursor) is
      begin
         Lists.Update_Element (This.Objects, I, Call'Access);
      end Iter;
   begin
      Lists.Iterate (This.Objects, Iter'Access);
   end Signal;

end Agpl.Generic_Messenger;
