package body Adafn.Errors is

   -------------------------
   -- Create_From_Message --
   -------------------------

   function Create_From_Message
     (Msg : Message.Object'Class)
      return For_Request
   is
   begin
      return (Code  => Error_Codes'Value (Msg.Value ("Code")),
              Fatal => Boolean'Value (Msg.Value ("Fatal")));
   end Create_From_Message;

   -------------------------
   -- Create_From_Message --
   -------------------------

   function Create_From_Message
     (Msg : Message.Object'Class)
      return For_Insert
   is
   begin
      return (Code  => Error_Codes'Value (Msg.Value ("Code")),
              Fatal => Boolean'Value (Msg.Value ("Fatal")));
   end Create_From_Message;

end Adafn.Errors;
