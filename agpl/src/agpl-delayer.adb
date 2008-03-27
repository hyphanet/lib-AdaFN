--  This object will force a delay if no enough time has elapsed.

package body Agpl.Delayer is

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   protected body Object is

      -------------
      -- Request --
      -------------
      --  Will return when the next slot of time has been reached.
      procedure Request is
         use Calendar;
         Now : constant Time := Clock;
      begin
         if Next_run > Now then
            delay until Next_run;
            Next_run := Next_run + Gap;
         else
            Next_run := Now + Gap;
         end if;
      end Request;

   end Object;

end Agpl.Delayer;
