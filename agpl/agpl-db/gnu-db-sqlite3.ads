-------------------------------------------------------------------------------
--									                                                  --
--  Filename        : $Source: /cvsroot/gnade/gnade/dbi/sqlite3/gnu-db-sqlite3.ads,v $
--  Description     : SQLite Base Package                                    --
--  Author          : Ching Bon Lam                                          --
--  Created         : 26.7.2003                                              --
--  Last Modified By: $Author: merdmann $				                             --
--  Last Modified On: $Date: 2004/12/28 21:31:38 $		                       --
--  Status          : $State: Exp $					                             --
--									                                                  --
--  Copyright (C) 2003 Ching Bon Lam                                         --
--                                                                           --
--  GNADE is free software;  you can redistribute it  and/or modify it under --
--  terms of the  GNU General Public License as published  by the Free Soft- --
--  ware  Foundation;  either version 2,  or (at your option) any later ver- --
--  sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
--  OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
--  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
--  for  more details.  You should have  received  a copy of the GNU General --
--  Public License  distributed with GNAT;  see file COPYING.  If not, write --
--  to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
--  MA 02111-1307, USA.                                                      --
--                                                                           --
--  As a special exception,  if other files  instantiate  generics from this --
--  unit, or you link  this unit with other files  to produce an executable, --
--  this  unit  does not  by itself cause  the resulting  executable  to  be --
--  covered  by the  GNU  General  Public  License.  This exception does not --
--  however invalidate  any other reasons why  the executable file  might be --
--  covered by the  GNU Public License.                                      --
--                                                                           --
--  Functional Description                                                   --
--  ======================                                                   --
--  Thick binding to sqlite (http://www.sqlite.org).                         --
--                                                                           --
--  Restrictions                                                             --
--  ============                                                             --
--  None                                                                     --
--                                                                           --
--  References                                                               --
--  ==========                                                               --
--  Sqlite homepage : http://www.sqlite.org                                  --
--                                                                           --
--  Contact                                                                  --
--  =======                                                                  --
--  Error reports shall be handled via http://gnade.sourceforge.net          --
--  Features and ideas via: gnade-develop@lists.sourceforge.net              --
--                                                                           --
--  Author contact:                                                          --
--    Ching Bon Lam  <cblam@gmx.net>                                         --
--									                                                  --
-------------------------------------------------------------------------------

with Ada.Finalization;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Interfaces.C;

package GNU.DB.SQLite3 is

   type Object is private;
   type Handle is access all Object;
   -- sqlite3.h:47
   -- typedef struct sqlite3 sqlite3;

   ----------------------------------------------------------------------------
   -- return values -----------------------------------------------------------
   ----------------------------------------------------------------------------

   type Return_Value is (
   -- sqlite3.h:133-161
      SQLITE_OK         , -- Successful result
      SQLITE_ERROR      , -- SQL error or missing database
      SQLITE_INTERNAL   , -- An internal logic error in SQLite
      SQLITE_PERM       , -- Access permission denied
      SQLITE_ABORT      , -- Callback routine requested an abort
      SQLITE_BUSY       , -- The database file is locked
      SQLITE_LOCKED     , -- A table in the database is locked
      SQLITE_NOMEM      , -- A malloc() failed
      SQLITE_READONLY   , -- Attempt to write a readonly database
      SQLITE_INTERRUPT  , -- Operation terminated by sqlite3_interrupt()
      SQLITE_IOERR      , -- Some kind of disk I/O error occurred
      SQLITE_CORRUPT    , -- The database disk image is malformed
      SQLITE_NOTFOUND   , -- (Internal Only) Table or record not found
      SQLITE_FULL       , -- Insertion failed because database is full
      SQLITE_CANTOPEN   , -- Unable to open the database file
      SQLITE_PROTOCOL   , -- Database lock protocol error
      SQLITE_EMPTY      , -- Database is empty
      SQLITE_SCHEMA     , -- The database schema changed
      SQLITE_TOOBIG     , -- Too much data for one row of a table
      SQLITE_CONSTRAINT , -- Abort due to contraint violation
      SQLITE_MISMATCH   , -- Data type mismatch
      SQLITE_MISUSE     , -- Library used incorrectly
      SQLITE_NOLFS      , -- Uses OS features not supported on host
      SQLITE_AUTH       , -- Authorization denied
      SQLITE_FORMAT     , -- Auxiliary database format error
      SQLITE_RANGE      , -- 2nd parameter to sqlite3_bind out of range
      SQLITE_NOTADB     , -- File opened that is not a database file
      SQLITE_ROW        , -- sqlite3_step() has another row ready
      SQLITE_DONE       , -- sqlite3_step() has finished executing
      UNDEFINED_ERROR
   );

   ----------------------------------------------------------------------------
   -- Other important types ---------------------------------------------------
   ----------------------------------------------------------------------------

   type Statement is private;
   type Statement_Reference is access all Statement;
   -- sqlite3.h:554
   -- typedef struct sqlite3_stmt sqlite3_stmt;

   type uint64 is mod 2 ** 64;
   type int64 is
      range -(2 ** (64 - 1)) ..
            +(2 ** (64 - 1) - 1);
   -- sqlite3.h:55

   package String_Vectors
      is new Ada.Containers.Indefinite_Vectors(natural,
                                               string);

   package Wide_String_Vectors
      is new Ada.Containers.Indefinite_Vectors(natural,
                                               wide_string);

   type Callback is access function
     (argc        : integer;
      argv        : String_Vectors.Vector;
      columnNames : String_Vectors.Vector)
      return Return_Value;

   type Callback16 is access function
     (argc        : integer;
      argv        : Wide_String_Vectors.Vector;
      columnNames : Wide_String_Vectors.Vector)
      return Return_Value;

   package String_Tables
      is new Ada.Containers.Vectors(natural,
                                    String_Vectors.Vector,
                                    String_Vectors."=");

   package Wide_String_Tables
      is new Ada.Containers.Vectors(natural,
                                    Wide_String_Vectors.Vector,
                                    Wide_String_Vectors."=");

   type Table_Reference is access all String_Tables.Vector;
   type Table16_Reference is access all Wide_String_Tables.Vector;

   ----------------------------------------------------------------------------
   -- Basic operations --------------------------------------------------------
   ----------------------------------------------------------------------------

   function Close
     (Self : Handle)
      return Return_Value;
   -- sqlite3.h:75
   -- int sqlite3_close(sqlite3 *);

   function Last_Insert_Rowid
     (Self : Handle)
      return uint64;
   -- sqlite3.h:172
   -- sqlite_int64 sqlite3_last_insert_rowid(sqlite3*);

   function Changes
     (Self : Handle)
      return integer;
   -- sqlite3.h:194
   -- int sqlite3_changes(sqlite3*);

   function Total_Changes
     (Self : Handle)
      return integer;
   -- sqlite3.h:212
   -- int sqlite3_total_changes(sqlite3*);

   procedure Interrupt
     (Self : Handle);
   -- sqlite3.h:220
   -- void sqlite3_interrupt(sqlite3*);

   function Complete
     (sql : string)
      return boolean;
   -- sqlite3.h:233
   -- int sqlite3_complete(const char *sql);

   function Complete16
     (sql : wide_string)
      return boolean;
   -- sqlite3.h:234
   -- int sqlite3_complete16(const void *sql);

   function Open
     (Self     : Handle;
      Filename : string)
      return Return_Value;
   -- sqlite3.h:504
   -- int sqlite3_open(
   --   const char *filename,   /* Database filename (UTF-8) */
   --   sqlite3 **ppDb          /* OUT: SQLite db handle */
   -- );

   function open16
     (Self     : Handle;
      Filename : wide_string)
      return Return_Value;
   -- sqlite3.h:508
   -- int sqlite3_open16(
   --   const void *filename,   /* Database filename (UTF-16) */
   --   sqlite3 **ppDb          /* OUT: SQLite db handle */
   -- );

   ----------------------------------------------------------------------------
   -- Exec --------------------------------------------------------------------
   ----------------------------------------------------------------------------

   generic
      type Data_Type is private;
      type Data_Handle is access all Data_Type;
   package Generic_Exec is

      type Callback is access function
        (pArg        : Data_Handle;
         argc        : integer;
         argv        : String_Vectors.Vector;
         columnNames : String_Vectors.Vector)
         return Return_Value;
      -- sqlite3.h:80
      -- typedef int (*sqlite3_callback)(void*,int,char**, char**);

      function Exec
        (Self    : Handle;
         sql     : string;
         cb      : Callback;
         arg     : Data_Handle)
         return Return_Value;
      -- sqlite3.h:122
      -- int sqlite3_exec(
      --   sqlite3*,         /* An open database */
      --   const char *sql,  /* SQL to be executed */
      --   sqlite3_callback, /* Callback function */
      --   void *,           /* 1st argument to callback function */
      --   char **errmsg     /* Error msg written here */
      -- );
   end Generic_Exec;

   generic
      type Data_Type is private;
      type Data_Handle is access all Data_Type;
   package Generic_Exec16 is

      type Callback16 is access function
        (pArg        : Data_Handle;
         argc        : integer;
         argv        : Wide_String_Vectors.Vector;
         columnNames : Wide_String_Vectors.Vector)
         return Return_Value;

      function Exec16
        (Self    : Handle;
         sql     : wide_string;
         cb      : Callback16;
         arg     : Data_Handle)
         return Return_Value;
   end Generic_Exec16;

   function Exec
     (Self : Handle;
      sql  : string;
      cb   : Callback)
      return Return_Value;

   function Exec16
     (Self : Handle;
      sql  : wide_string;
      cb   : Callback16)
      return Return_Value;

   function Exec
     (Self : Handle;
      sql  : string)
      return Return_Value;

   function Exec16
     (Self : Handle;
      sql  : wide_string)
      return Return_Value;

   ----------------------------------------------------------------------------
   -- Tables ------------------------------------------------------------------
   ----------------------------------------------------------------------------

   function Get_Table
     (Self  : Handle;
      Sql   : string;
      table : Table_Reference)
      return Return_Value;

   function Get_Table16
     (Self  : Handle;
      Sql   : wide_string;
      table : Table16_Reference)
      return Return_Value;

   function Row_Count
     (Self : Table_Reference)
      return natural;

   function Column_Count
     (Self : Table_Reference;
      Row  : natural)
      return natural;

   function Element
     (Self : Table_Reference;
      row  : natural;
      col  : natural)
      return string;

   function Element16
     (Self : Table16_Reference;
      row  : natural;
      col  : natural)
      return wide_string;


   ----------------------------------------------------------------------------
   -- Error stuff -------------------------------------------------------------
   ----------------------------------------------------------------------------

   function Errcode
     (Self : Handle)
      return integer;
   -- sqlite3.h:528
   -- int sqlite3_errcode(sqlite3 *db);

   function Errmsg
     (Self : Handle)
      return string;
   -- sqlite3.h:538
   -- const char *sqlite3_errmsg(sqlite3*);

   function Errmsg16
     (Self : Handle)
      return wide_string;
   -- sqlite3.h:548
   -- const void *sqlite3_errmsg16(sqlite3*);

   ----------------------------------------------------------------------------
   -- Stepping ----------------------------------------------------------------
   ----------------------------------------------------------------------------

   SQLITE_INTEGER : constant integer := 1;
   SQLITE_FLOAT   : constant integer := 2;
   SQLITE_TEXT    : constant integer := 3;
   SQLITE_BLOB    : constant integer := 4;
   SQLITE_NULL    : constant integer := 5;

   function prepare
     (self : Handle;
      sql  : string;
      stmt : Statement_Reference)
      return Return_Value;
   -- sqlite3.h:581
   -- int sqlite3_prepare(
   --   sqlite3 *db,            /* Database handle */
   --   const char *zSql,       /* SQL statement, UTF-8 encoded */
   --   int nBytes,             /* Length of zSql in bytes. */
   --   sqlite3_stmt **ppStmt,  /* OUT: Statement handle */
   --   const char **pzTail     /* OUT: Pointer to unused portion of zSql */
   -- );

   function prepare16
     (self : Handle;
      sql  : wide_string;
      stmt : Statement_Reference)
      return Return_Value;
   -- sqlite3.h:588
   -- int sqlite3_prepare16(
   --   sqlite3 *db,            /* Database handle */
   --   const void *zSql,       /* SQL statement, UTF-16 encoded */
   --   int nBytes,             /* Length of zSql in bytes. */
   --   sqlite3_stmt **ppStmt,  /* OUT: Statement handle */
   --   const void **pzTail     /* OUT: Pointer to unused portion of zSql */
   -- );


   -- function bind_blob
   --   (stmt : Statement_Reference;
   --    N    : integer;
   --    B    : something)
   --    return Return_Value;
   -- sqlite3.h:626
   -- int sqlite3_bind_blob(sqlite3_stmt*, int, const void*, int n, void(*)(void*));

   function bind_double
     (stmt : Statement_Reference;
      N    : integer;
      D    : long_float)
      return Return_Value;
   -- sqlite3.h:627
   -- int sqlite3_bind_double(sqlite3_stmt*, int, double);

   function bind_int
     (stmt : Statement_Reference;
      N    : integer;
      I    : integer)
      return Return_Value;
   -- sqlite3.h:628
   -- int sqlite3_bind_int(sqlite3_stmt*, int, int);

   function bind_int64
     (stmt : Statement_Reference;
      N    : integer;
      I    : int64)
      return Return_Value;
   -- sqlite3.h:629
   -- int sqlite3_bind_int64(sqlite3_stmt*, int, sqlite_int64);

   function bind_null
     (stmt : Statement_Reference;
      N    : integer)
      return Return_Value;
   -- sqlite3.h:630
   -- int sqlite3_bind_null(sqlite3_stmt*, int);

   function bind_text
     (stmt : Statement_Reference;
      N    : integer;
      T    : string)
      return Return_Value;
   -- sqlite3.h:631
   -- int sqlite3_bind_text(sqlite3_stmt*, int, const char*, int n, void(*)(void*));

   function bind_text16
     (stmt : Statement_Reference;
      N    : integer;
      T    : wide_string)
      return Return_Value;
   -- sqlite3.h:632
   -- int sqlite3_bind_text16(sqlite3_stmt*, int, const void*, int, void(*)(void*));

   -- function bind_value
   --   (stmt : Statement_Reference;
   --    N    : integer;
   --    V    : Value)
   --    return Return_Value;
   -- sqlite3.h:633
   -- int sqlite3_bind_value(sqlite3_stmt*, int, const sqlite3_value*);

   function column_count
     (stmt : Statement_Reference)
      return integer;
   -- sqlite3.h:661
   -- int sqlite3_column_count(sqlite3_stmt *pStmt);

   function column_name
     (stmt : Statement_Reference;
      iCol : integer)
      return string;
   -- sqlite3.h:669
   -- const char *sqlite3_column_name(sqlite3_stmt*,int);

   function column_name16
     (stmt : Statement_Reference;
      iCol : integer)
      return wide_string;
   -- sqlite3.h:670
   -- const void *sqlite3_column_name16(sqlite3_stmt*,int);

   function column_decltype
     (stmt : Statement_Reference;
      iCol : integer)
      return string;
   -- sqlite3.h:690
   -- const char *sqlite3_column_decltype(sqlite3_stmt *, int i);

   function column_decltype16
     (stmt : Statement_Reference;
      iCol : integer)
      return wide_string;
   -- sqlite3.h:710
   -- const void *sqlite3_column_decltype16(sqlite3_stmt*,int);

   function step
     (stmt : Statement_Reference)
      return Return_Value;
   -- sqlite3.h:744
   -- int sqlite3_step(sqlite3_stmt*);

   function data_count
     (stmt : Statement_Reference)
      return integer;
   -- sqlite3.h:755
   -- int sqlite3_data_count(sqlite3_stmt *pStmt);

   function column_int
     (stmt : Statement_Reference;
      iCol : integer)
      return integer;
   -- sqlite3.h:828
   -- int sqlite3_column_int(sqlite3_stmt*, int iCol);

   function column_text
     (stmt : Statement_Reference;
      iCol : integer)
      return string;
   -- sqlite3.h:830
   --const unsigned char *sqlite3_column_text(sqlite3_stmt*, int iCol);

   function column_text16
     (stmt : Statement_Reference;
      iCol : integer)
      return wide_string;
   -- sqlite3.h:831
   --const void *sqlite3_column_text16(sqlite3_stmt*, int iCol);

   function column_type
     (stmt : Statement_Reference;
      iCol : integer)
      return integer;
   -- sqlite3.h:832
   -- int sqlite3_column_type(sqlite3_stmt*, int iCol);

   function finalize
     (stmt : Statement_Reference)
      return Return_Value;
   -- sqlite3.h:848
   -- int sqlite3_finalize(sqlite3_stmt *pStmt);

   function reset
     (stmt : Statement_Reference)
      return Return_Value;
   -- sqlite3.h:857
   -- int sqlite3_reset(sqlite3_stmt *pStmt);

   ----------------------------------------------------------------------------
   -- PRIVATE -----------------------------------------------------------------
   ----------------------------------------------------------------------------

   private

   type DB_Type is limited null record;
   type DB_Access is access all DB_Type;
   for DB_Access'Storage_Size use 0;
   pragma Convention (C, DB_Access);

   type Object is record
      db : aliased DB_Access := null;
   end record;
   -- sqlite3.h:47
   -- typedef struct sqlite3 sqlite3;

   type Stmt_Type is limited null record;
   type Stmt_Access is access all Stmt_Type;
   for Stmt_Access'Storage_Size use 0;
   pragma Convention (C, Stmt_Access);

   type Statement is record
      stmt : aliased Stmt_Access := null;
   end record;

end GNU.DB.SQLite3;
