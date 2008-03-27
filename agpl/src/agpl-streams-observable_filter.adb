 

--  Abstract class; A filter stream takes an access to a back stream from
--  which it can read/write, performing some extra operation/filtering if
--  needed.

--  This one is observable and takes an observable back end.

package body Agpl.Streams.Observable_Filter is

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   procedure Create (
      This : in out Stream_Type;
      Back : access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      This.Back := Agpl.Streams.Stream_Access (Back);
   end Create;

end Agpl.Streams.Observable_Filter;
