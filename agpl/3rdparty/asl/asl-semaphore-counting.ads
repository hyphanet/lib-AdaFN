-- The Ada Structured Library - A set of container classes and general
--   tools for use with Ada95.
-- Copyright (C) 1998-1999  Corey Minyard (minyard@acm.org)
--
-- This library is free software; you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the
-- Free Software Foundation; either version 2 of the License, or (at your
-- option) any later version.
--
-- This library is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License along
-- with this library; if not, write to the Free Software Foundation, Inc.,
-- 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA
--
-- As a special exception, if other files instantiate generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License.  This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.
--

-- A standard counting semaphore.  The initial count is specified when the
-- semaphore is declared.  The Take operation will block is the count is
-- zero, otherwise Take will decremenet the count.  Give will always
-- increment the count.

package Asl.Semaphore.Counting is

   type Object (Initial_Count : Natural) is
     new Asl.Semaphore.Object with private;
   type Object_Ptr is access all Object;

   -- Claim the semaphore.  Will block until the semaphore is available.
   procedure Take (O : in out Object);

   -- Release the semaphore.
   procedure Give (O : in out Object);

   -- Try to take the semaphore, but return an error if not successful.
   -- Returns True if the semaphore was claimed and False if not.  This
   -- will wait up to Timeout time for the semaphore to become available.
   procedure Try_To_Take (O       : in out Object;
                          Success : out Boolean;
                          Timeout : in Duration := 0.0);

private

   protected type Mutex (Initial_Count : Natural) is
      entry Wait;
      procedure Release;
   private
      Count : Natural := Initial_Count;
   end Mutex;

   type Object (Initial_Count : Natural) is
     new Asl.Semaphore.Object with record
      The_Mutex : Mutex(Initial_Count);
   end record;

end Asl.Semaphore.Counting;
