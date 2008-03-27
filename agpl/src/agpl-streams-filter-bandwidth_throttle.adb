 

--  Filter stream which uses an Agpl.Bandwidth_Throttle to limit data rates.
--  Provides statistics about global data rate and "instantaneous" data rate.
--  The instantaneous data rate averaging can be configured.

with Agpl.Exceptions;

with Ada.Streams;

package body Agpl.Streams.Filter.Bandwidth_Throttle is

   use type Agpl.Bandwidth_Throttle.Object_Access;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   procedure Create (
      This              : in out Stream_Type;
      Back              : access Ada.Streams.Root_Stream_Type'Class;
      Throttle_In       : in     Agpl.Bandwidth_Throttle.Object_Access;
      Throttle_Out      : in     Agpl.Bandwidth_Throttle.Object_Access;
      Max_Buffer_Size   : in     Stream_Element_Count :=
                                    Stream_Element_Count'Last;
      Initial_Size      : in     Stream_Element_Count := 4096;
      Grow_Percent      : in     Natural              := 50;
      Averaging_Slots   : in     Natural              := 5;
      Slot_Duration     : in     Natural              := 1000)
   is
   begin
      Reset (This);
      Filter.Create (Filter.Stream_Type (This), Back);
      This.Avg_In  := new Avg.Object (Averaging_Slots, Slot_Duration);
      This.Avg_Out := new Avg.Object (Averaging_Slots, Slot_Duration);
      Circular.Create (
         This.Buf_Out,
         Max_Buffer_Size,
         Initial_Size,
         Grow_Percent);
      if Throttle_In /= null then
         This.Throttle_In := Throttle_In;
      else
         This.Throttle_In := Agpl.Bandwidth_Throttle.Unlimited'Access;
      end if;
      if Throttle_Out /= null then
         This.Throttle_Out := Throttle_In;
      else
         This.Throttle_Out := Agpl.Bandwidth_Throttle.Unlimited'Access;
      end if;
   end Create;

   ------------------------------------------------------------------------
   -- Read                                                               --
   ------------------------------------------------------------------------
   --  Reserves from the throttle and reads that from back.
   procedure Read (
      This : in out Stream_Type;
      Item :    out Ada.Streams.Stream_Element_Array;
      Last :    out Ada.Streams.Stream_Element_Offset)
   is
      Aw, Ex : Stream_Element_Count;
   begin
      This.Throttle_In.Commit (Item'Length, Aw);
      if Aw < Item'Length then
         This.Throttle_In.Commit (Item'Length - Aw, Ex, Extra => True);
      else
         Ex := 0;
      end if;

      Ada.Streams.Read (
         This.Back.all,
         Item (Item'First .. Item'First + Aw + Ex - 1),
         Last);

      This.Total_Read := This.Total_Read + Last - Item'First + 1;
      Avg.Push (This.Avg_In.all, Float (Last - Item'First + 1));
   end Read;

   ------------------------------------------------------------------------
   -- Write                                                              --
   ------------------------------------------------------------------------
   --  Non-blocking. Will cache data which cannot be immediately written.
   procedure Write (
      This : in out Stream_Type;
      Item : in     Ada.Streams.Stream_Element_Array)
   is
      Aw, Ex : Stream_Element_Count;
   begin
      if Get_Buffered_Write_Count (This) > 0 then
         Circular.Write (This.Buf_Out, Item);
         Flush (This);
      else
         --  We could always cache and then flush, but this way when not
         --  throttling we are more efficient avoiding two memory copies.
         This.Throttle_Out.Commit (Item'Length, Aw);
         if Aw < Item'Length then
            This.Throttle_Out.Commit (Item'Length - Aw, Ex, Extra => True);
         else
            Ex := 0;
         end if;

         Ada.Streams.Write (
            This.Back.all,
            Item (Item'First .. Item'First + Aw + Ex - 1));
         This.Total_Written := This.Total_Written + Aw + Ex;
         Avg.Push (This.Avg_Out.all, Float (Aw + Ex));

         if Aw + Ex < Item'Length then
            Circular.Write (
               This.Buf_Out,
               Item (Item'First + Aw + Ex .. Item'Last));
         end if;
      end if;
   end Write;

   ------------------------------------------------------------------------
   -- Prefetch                                                           --
   ------------------------------------------------------------------------
   --  A "pull" procedure to get data into de filter from the "read" end,
   --  but without needing the data to be actually read from the filter.
   procedure Prefetch (
      This  : in out Stream_Type;
      Count : in     Stream_Element_Count)
   is
      pragma Unreferenced (This);
      pragma Unreferenced (Count);
   begin
      raise Exceptions.Unimplemented;
   end Prefetch;

   ------------------------------------------------------------------------
   -- Reset                                                              --
   ------------------------------------------------------------------------
   --  Cleans everything inside, including stats.
   procedure Reset (This : in out Stream_Type) is
   begin
      Circular.Reset (This.Buf_Out);
      This.Total_Read       := 0;
      This.Total_Written    := 0;
      This.Start            := Calendar.Clock;
   end Reset;

   ------------------------------------------------------------------------
   -- Flush                                                              --
   ------------------------------------------------------------------------
   --  Tries to write again data which was cached.
   procedure Flush (This : in out Stream_type) is
      Avail  : constant Stream_Element_Count :=
         Circular.Available_Read (This.Buf_Out);
      Aw, Ex : Stream_Element_Count;
   begin
      if Avail > 0 then
         This.Throttle_Out.Commit (Avail, Aw);
         if Aw < Avail then
            This.Throttle_Out.Commit (Avail - Aw, Ex, Extra => True);
         else
            Ex := 0;
         end if;
         declare
            Item : Stream_Element_Array (1 .. Aw + Ex);
            Last : Stream_Element_Offset;
         begin
            Circular.Peek (This.Buf_Out, Item, Last);
            Ada.Streams.Write (This.Back.all, Item (Item'First .. Last));
            --  If no fail in the writting, mark the chunk as read:
            Circular.Skip (This.Buf_Out, Last - Item'First + 1);

            This.Total_Written := This.Total_Written + Last - Item'First + 1;
            Avg.Push (This.Avg_Out.all, Float (Last - Item'First + 1));
         end;
      end if;
   end Flush;

   ------------------------------------------------------------------------
   -- Available_Read                                                     --
   ------------------------------------------------------------------------
   --  Unlimited
   function Available_Read (This : in Stream_Type)
      return Stream_Element_Count
   is
      pragma Unreferenced (This);
   begin
      raise Unknown_Availability;
      return 0;
   end Available_Read;

   ------------------------------------------------------------------------
   -- Available_Write                                                    --
   ------------------------------------------------------------------------
   --  Returns Max_Memory_Usage
   function Available_Write (This : in Stream_Type)
      return Stream_Element_Count
   is
   begin
      return Circular.Available_Write (This.Buf_Out);
   end Available_Write;

   ------------------------------------------------------------------------
   -- Get_Total_Read                                                     --
   ------------------------------------------------------------------------
   --  How many bytes have been read from this throttle.
   function Get_Total_Read (This : in Stream_Type) return Stream_Element_Count
   is
   begin
      return This.Total_Read;
   end Get_Total_Read;

   ------------------------------------------------------------------------
   -- Get_Buffered_Write_Count                                           --
   ------------------------------------------------------------------------
   --  Returns the amount of written buffered data.
   function Get_Buffered_Write_Count (This : in Stream_Type)
      return Stream_Element_Count is
   begin
      return Circular.Available_Read (This.Buf_Out);
   end Get_Buffered_Write_Count;

   ------------------------------------------------------------------------
   -- Get_Current_Write_Rate                                             --
   ------------------------------------------------------------------------
   --  Gives the instantaneous writting speed.
   function Get_Current_Write_Rate (This : access Stream_Type) return Types.Data_Rate is
      Rate : Float;
   begin
      Avg.Average (This.Avg_Out.all, Rate);
      return Types.Data_Rate (Rate);
   end Get_Current_Write_Rate;

   ------------------------------------------------------------------------
   -- Get_Current_Read_Rate                                              --
   ------------------------------------------------------------------------
   --  Gives the reading speed since creation or last reset.
   function Get_Current_Read_Rate (This : access Stream_Type) return Types.Data_Rate is
      Rate : Float;
   begin
      Avg.Average (This.Avg_In.all, Rate);
      return Types.Data_Rate (Rate);
   end Get_Current_Read_Rate;

   ------------------------------------------------------------------------
   -- Get_Global_Write_Rate                                              --
   ------------------------------------------------------------------------
   --  Gives the writting speed since creation or last reset.
   function Get_Global_Write_Rate (This : in Stream_Type) return Types.Data_Rate is
   begin
      return
         Types.Data_Rate (This.Total_Written) /
         Types.Data_Rate (Calendar.Clock - This.Start);
   end Get_Global_Write_Rate;

   ------------------------------------------------------------------------
   -- Get_Global_Read_Rate                                               --
   ------------------------------------------------------------------------
   --  Gives the reading speed since creation or last reset.
   function Get_Global_Read_Rate (This : in Stream_Type) return Types.Data_Rate is
   begin
      return
         Types.Data_Rate (This.Total_Read) /
         Types.Data_Rate (Calendar.Clock - This.Start);
   end Get_Global_Read_Rate;

   ------------------------------------------------------------------------
   -- Finalize                                                           --
   ------------------------------------------------------------------------
   procedure Finalize (This : in out Stream_Type) is
   begin
      Avg.Free (This.Avg_In);
      Avg.Free (This.Avg_Out);
   end Finalize;

end Agpl.Streams.Filter.Bandwidth_Throttle;
