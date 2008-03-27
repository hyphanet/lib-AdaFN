with Adafn.Message;

with Agpl.Containers.Integer_Sets;
with Agpl.Containers.Utils;
use Agpl;

package Adafn.Errors is

   type Error_Codes is new Natural;

   type For_Request is record
      Fatal : Boolean;
      Code  : Error_Codes;
   end record;

   type For_Insert is new For_Request;

   function Create_From_Message (Msg : Message.Object'Class) return For_Request;
   function Create_From_Message (Msg : Message.Object'Class) return For_Insert;

   --  Request errors
   Too_Many_Path_Components : constant := 11;
   Fetch_Data_Not_Found     : constant := 13;
   Fetch_Route_Not_Found    : constant := 14;
   Rejected_Overload        : constant := 15;
   Transfer_Failed          : constant := 18;
   Fetch_Permanent_Redirect : constant := 27;
   Fetch_All_Data_Not_Found : constant := 28;

   --  Our own start at the end
   Fetch_Bad_Data            : constant := 999;
   Fetch_Unhandled_Exception : constant := 998;

   --  Errors that aren't the fault of the key but the node
   Retriable_Errors : constant Containers.Integer_Sets.Set :=
                        Containers.Utils.To_Set
                          ((Fetch_Route_Not_Found,
                           Rejected_Overload,
                           Transfer_Failed,
                           Fetch_All_Data_Not_Found));

   Chk_Unrecoverable_Errors : constant Containers.Integer_Sets.Set :=
                                Containers.Utils.To_Set
                                  ((1,
                                   2,
                                   3,
                                   4,
                                   5,
                                   6,
                                   7,
                                   8,
                                   9,
                                   10,
                                   16,
                                   20,
                                   21,
                                   22,
                                   23,
                                   999));

end Adafn.Errors;
