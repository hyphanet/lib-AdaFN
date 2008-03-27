 

package body Agpl.Average_queue is

   ----------
   -- Push --
   ----------

   procedure Push (this: in out Object; New_item: in Item) is
   begin
      This.Sum := This.Sum + New_Item;

      if This.Length >= This.Data'Length then
         --  Remove item to be dropped from acummulator:
         This.Sum := This.Sum - This.Data (This.Pos);
      end if;

      This.Data (This.Pos) := New_Item;
      This.Pos             := This.Pos + 1;

      if This.Pos > This.Data'Last then
         This.Pos := This.Data'First;
      end if;

      if This.Length < This.Data'Length then
         This.Length := This.Length + 1;
      end if;
   end Push;

   -------------
   -- Average --
   -------------

   function Average (this: in Object) return Item is
   begin
      if this.Length = 0 then
         raise No_data;
      else
         return This.Sum / Item (This.Length);
      end if;
   end Average;

   --------------
   -- Is_empty --
   --------------

   function Is_empty (This : in Object) return Boolean is
   begin
      return This.Length = 0;
   end Is_empty;

   -----------
   -- Clear --
   -----------

   procedure Clear (This : out Object) is
   begin
      This.Length := 0;
      This.Pos    := 1;
      This.Sum    := 0.0;
   end Clear;

end Agpl.Average_queue;
