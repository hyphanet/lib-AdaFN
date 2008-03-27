 

--  Protected container for values of any private type.

with Agpl.Protected_Value;

with Ada.Calendar;

package Agpl.Protected_Values.Time is
   new Agpl.Protected_Value (Ada.Calendar.Time);
