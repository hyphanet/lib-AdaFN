 

--  Abstract class; A filter stream takes an access to a back stream from
--  which it can read/write, performing some extra operation/filtering if
--  needed.

--  This one is observable and takes an observable back end.

with Agpl.Streams.Observable;

with Ada.Streams;

package Agpl.Streams.Observable_Filter is

   pragma Preelaborate;

   ------------------------------------------------------------------------
   -- Stream_type                                                        --
   ------------------------------------------------------------------------
   type Stream_Type
   is abstract new Agpl.Streams.Observable.Stream_Type with private;

   type Stream_Access is access all Stream_type;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   procedure Create (
      This : in out Stream_Type;
      Back : access Ada.Streams.Root_Stream_Type'Class);

private

   type Stream_Type
   is abstract new Agpl.Streams.Observable.Stream_Type with record
      Back : Agpl.Streams.Stream_Access;
   end record;

end Agpl.Streams.Observable_Filter;
