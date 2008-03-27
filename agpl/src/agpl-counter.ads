 

package Agpl.Counter is

   pragma Preelaborate;

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   protected type Object (Initial_Value : Integer := 0) is
      procedure Add (Increment : in Integer := 1);
      procedure Reset (Val     : in Integer := 0);
      function  Val return Integer;
   private
      Value : Integer := Initial_Value;
   end Object;

   type Object_Access is access all Object;

end Agpl.Counter;
