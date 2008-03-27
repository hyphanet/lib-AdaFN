--  GENERIC EXCHANGE PROCEDURE
   --------------------------

--  Last Modified By: Mats Weber
--  Last Modified On: Mon Sep  8 12:04:16 1997
--  Update Count    : 2

--  Creation : 17-NOV-1989 by Mats Weber, taken from package Utilities.


generic
   type Item (<>) is private;
procedure Exchange (X, Y : in out Item);
------------------
--  Exchanges X and Y.

pragma Preelaborate (Exchange);
pragma Inline(Exchange);
