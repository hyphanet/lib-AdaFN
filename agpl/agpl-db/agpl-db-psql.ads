

--  Utilities for PostgreSQL databases

with Agpl.Containers.String_Sets;

with Gnu.Db.Postgresql; use Gnu.Db.Postgresql;

with Ada.Finalization;

package Agpl.Db.Psql is

   pragma Preelaborate;

   type Database_Access is access all Database;

   function Connect (Db   : String;
                     User : String;
                     Pass : String;
                     Host : String  := "127.0.0.1";
                     Port : Natural := 5432;
                     Ssl  : Boolean := True) return Database_Access;
   --  Connection status is not checked; caller must check it.

   procedure Check_Query (R : Result; Db : Database);
   --  Asserts that the query hasn't returned error
   --  Also issues a rollback

   procedure Query (R : out Result; Db : Database; Q : String);
   --  Performs the query and checks for failure

   function Query (Db : Database; Sql : String) return String;
   --  Get the first column of the first row of a query.
   --  Useful for count and the like

   procedure Begin_Transaction (Db : Database);

   procedure Commit_Transaction (Db : Database);
   procedure Commit (Db : Database) renames Commit_Transaction;

   procedure Rollback_Transaction (Db : Database);
   procedure Rollback (Db : Database) renames Rollback_Transaction;

   type Transaction (Db : access Database) is limited private;
   --  Controlled type that will issue a Begin on creation and a Commit on
   --  destruction.

   function Escape (This : String) return String;
   function Unescape (This : String) return String;
   --  The ' character!

   function Load_Table_Column (Db : Database; Table : String; Column : Positive)
                               return Agpl.Containers.String_Sets.Set;

   function Bool_Value (Str : String) return Boolean; pragma Inline (Bool_Value);
   --  Translate 't'/'f' into boolean
private

   type Transaction (Db : access Database) is
     new Ada.Finalization.Limited_Controlled with null record;

   procedure Initialize (This : in out Transaction);
   procedure Finalize   (This : in out Transaction);

end Agpl.Db.Psql;
