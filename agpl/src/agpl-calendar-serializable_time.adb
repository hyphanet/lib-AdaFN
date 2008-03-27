package body Agpl.Calendar.Serializable_Time is

   ---------
   -- "+" --
   ---------

   function "+" (This : in Object) return Ada.Calendar.Time is
   begin
      return Ada.Calendar.Time (This);
   end "+";

   function "+" (This : in Ada.Calendar.Time) return Object is
   begin
      return Object (This);
   end "+";

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      This   :    out Object)
   is
      Elapsed : Duration;
   begin
      Duration'Read (Stream, Elapsed);
      This := Epoch + Elapsed;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      This   : in     Object)
   is
   begin
      Duration'Write (Stream, This - Epoch);
   end Write;

end Agpl.Calendar.Serializable_Time;
