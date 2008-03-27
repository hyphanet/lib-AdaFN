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

with Ada.Unchecked_Deallocation;
with Ada.Text_IO;

with Interfaces.C;
with Interfaces.C.Strings;
with Interfaces.C.Pointers;

with System;

package body GNU.DB.SQLite3 is

   pragma Linker_Options("-lsqlite3");

   package C   renames Interfaces.C;
   package CS  renames Interfaces.C.Strings;

   ----------------------------------------------------------------------------
   -- local support -----------------------------------------------------------
   ----------------------------------------------------------------------------

   package C_Wide_Strings is
      new C.Pointers( C.size_T,
                      C.WChar_T,
                      C.WChar_Array,
                      C.Wide_Nul );
   package CWS renames C_Wide_Strings;

   function To_Ada
     (Thing : CWS.Pointer)
      return wide_string
   is
   begin
      return C.To_Ada(CWS.Value(Thing));
   end To_Ada;
   pragma Inline(To_Ada);

   function To_Ada
     (Thing : CS.chars_ptr)
      return string
   is
      use type CS.chars_ptr;
   begin
      if Thing = CS.Null_Ptr then
         return "";
      else
         return CS.Value (Thing);
      end if;
   end To_Ada;
   pragma Inline(To_Ada);


   function To_C
     (Thing : wide_string)
      return C.wchar_array
   is
   begin
      return C.To_C(Thing)(0 .. C.Size_T(Thing'Length));
   end To_C;
   pragma Inline(To_C);

   function int_to_ret_val
     (rc : integer)
      return Return_Value
   is
   begin
      case rc is
         when  0 => return SQLITE_OK;
         when  1 => return SQLITE_ERROR;
         when  2 => return SQLITE_INTERNAL;
         when  3 => return SQLITE_PERM;
         when  4 => return SQLITE_ABORT;
         when  5 => return SQLITE_BUSY;
         when  6 => return SQLITE_LOCKED;
         when  7 => return SQLITE_NOMEM;
         when  8 => return SQLITE_READONLY;
         when  9 => return SQLITE_INTERRUPT;
         when 10 => return SQLITE_IOERR;
         when 11 => return SQLITE_CORRUPT;
         when 12 => return SQLITE_NOTFOUND;
         when 13 => return SQLITE_FULL;
         when 14 => return SQLITE_CANTOPEN;
         when 15 => return SQLITE_PROTOCOL;
         when 16 => return SQLITE_EMPTY;
         when 17 => return SQLITE_SCHEMA;
         when 18 => return SQLITE_TOOBIG;
         when 19 => return SQLITE_CONSTRAINT;
         when 20 => return SQLITE_MISMATCH;
         when 21 => return SQLITE_MISUSE;
         when 22 => return SQLITE_NOLFS;
         when 23 => return SQLITE_AUTH;
         when 24 => return SQLITE_FORMAT;
         when 25 => return SQLITE_RANGE;
         when 26 => return SQLITE_NOTADB;
         when 100 => return SQLITE_ROW;
         when 101 => return SQLITE_DONE;
         when others => return UNDEFINED_ERROR;
      end case;
   end int_to_ret_val;

   function int_to_ret_val
     (int : C.int)
      return Return_Value
   is
   begin
      return int_to_ret_val(integer(int));
   end int_to_ret_val;

   -- pragma Import(C, SQLITE_STATIC, "SQLITE_STATIC");

   ----------------------------------------------------------------------------
   -- body --------------------------------------------------------------------
   ----------------------------------------------------------------------------

   -----------------
   -- bind_double --
   -----------------

   function bind_double
     (Stmt : Statement_Reference;
      N    : integer;
      D    : long_float)
      return Return_Value
   is

      function sqlite3_bind_double
        (stmt : Stmt_Access;
         N    : C.int;
         D    : C.double)
         return C.int;
      pragma Import(C, sqlite3_bind_double, "sqlite3_bind_double");
      -- sqlite3.h:627
      -- int sqlite3_bind_double(sqlite3_stmt*, int, double);

   begin

      return int_to_ret_val(sqlite3_bind_double(Stmt.stmt, C.int(N), C.double(D)));

   end bind_double;

   --------------
   -- bind_int --
   --------------

   function bind_int
     (stmt : Statement_Reference;
      N    : integer;
      I    : integer)
      return Return_Value
   is

      function sqlite3_bind_int
        (stmt : Stmt_Access;
         N    : C.int;
         I    : C.int)
         return C.int;
      pragma Import(C, sqlite3_bind_int, "sqlite3_bind_int");
      -- sqlite3.h:628
      -- int sqlite3_bind_int(sqlite3_stmt*, int, int);

   begin

      return int_to_ret_val(sqlite3_bind_int(Stmt.stmt, C.int(N), C.int(I)));

   end bind_int;

   ----------------
   -- bind_int64 --
   ----------------

   function bind_int64
     (stmt : Statement_Reference;
      N    : integer;
      I    : int64)
      return Return_Value
   is

      function sqlite3_bind_int64
        (stmt : Stmt_Access;
         N    : C.int;
         I    : int64)
         return C.int;
      pragma Import(C, sqlite3_bind_int64, "sqlite3_bind_int64");
      -- sqlite3.h:629
      -- int sqlite3_bind_int64(sqlite3_stmt*, int, sqlite_int64);

   begin

      return int_to_ret_val(sqlite3_bind_int64(Stmt.stmt, C.int(N), I));

   end bind_int64;

   ---------------
   -- bind_null --
   ---------------

   function bind_null
     (stmt : Statement_Reference;
      N    : integer)
      return Return_Value
   is

      function sqlite3_bind_null
        (stmt : Stmt_Access;
         N    : C.int)
         return C.int;
      pragma Import(C, sqlite3_bind_null, "sqlite3_bind_null");
      -- sqlite3.h:630
      -- int sqlite3_bind_null(sqlite3_stmt*, int);

   begin

      return int_to_ret_val(sqlite3_bind_null(Stmt.stmt, C.int(N)));

   end bind_null;

   ---------------
   -- bind_text --
   ---------------

   function bind_text
     (stmt : Statement_Reference;
      N    : integer;
      T    : string)
      return Return_Value
   is

      function sqlite3_bind_text
        (stmt : Stmt_Access;
         N    : C.int;
         T    : C.char_array;
         N2   : C.int;
         A    : System.Address)
         return C.int;
      pragma Import(C, sqlite3_bind_text, "sqlite3_bind_text");
      -- sqlite3.h:631
      -- int sqlite3_bind_text(sqlite3_stmt*, int, const char*, int n, void(*)(void*));

   begin

      return int_to_ret_val(sqlite3_bind_text(
         Stmt.stmt,
         C.int(N),
         C.To_C(T),
         0,
         System.Null_Address --SQLITE_STATIC
         ));

   end bind_text;

   -----------------
   -- bind_text16 --
   -----------------

   function bind_text16
     (stmt : Statement_Reference;
      N    : integer;
      T    : wide_string)
      return Return_Value
   is

      function sqlite3_bind_text16
        (stmt : Stmt_Access;
         N    : C.int;
         T    : C.wchar_array;
         N2   : C.int;
         A    : System.Address)
         return C.int;
      pragma Import(C, sqlite3_bind_text16, "sqlite3_bind_text16");
      -- sqlite3.h:632
      -- int sqlite3_bind_text16(sqlite3_stmt*, int, const void*, int, void(*)(void*));

   begin

      return int_to_ret_val(sqlite3_bind_text16(
         Stmt.stmt,
         C.int(N),
         To_C(T),
         0,
         System.Null_Address
         ));

   end bind_text16;

   -----------
   -- close --
   -----------

   function close
     (Self : Handle)
      return Return_Value
   is

   	function sqlite3_close
         (db : DB_Access)
          return C.int;
      pragma Import(C, sqlite3_close, "sqlite3_close");
      -- sqlite3.h:75
      -- int sqlite3_close(sqlite3 *);

      Return_Code : Return_Value;

   begin

      Return_Code := int_to_ret_val(sqlite3_close(Self.db));

     	if (Return_Code = SQLITE_OK) then
      	self.db := null;
     	end if;

     	return Return_Code;

   end close;

   -------------
   -- Changes --
   -------------

   function Changes
     (Self : Handle)
      return integer
   is

   	function sqlite3_changes
         (db : DB_Access)
          return C.int;
      pragma Import(C, sqlite3_changes, "sqlite3_changes");
      -- sqlite3.h:194
      -- int sqlite3_changes(sqlite3*);

   begin

      return integer(sqlite3_changes(Self.db));

   end Changes;

   ------------------
   -- column_count --
   ------------------

   function column_count
     (stmt : Statement_Reference)
      return integer
   is

      function sqlite3_column_count
        (stmt : Stmt_Access)
         return C.int;
      pragma Import(C, sqlite3_column_count, "sqlite3_column_count");
      -- sqlite3.h:661
      -- int sqlite3_column_count(sqlite3_stmt *pStmt);

   begin

      return integer(sqlite3_column_count(stmt.stmt));

   end column_count;

   ------------------
   -- column_count --
   ------------------

   function Column_Count
     (Self : Table_Reference;
      Row  : natural)
      return natural
   is

      use String_Vectors;
      use String_Tables;

   begin

      if Row <= Row_Count(Self) then
         return natural(Length(Element(Self.all, Row)));
      end if;

      return 0;

   end Column_Count;

   ---------------------
   -- column_decltype --
   ---------------------

   function column_decltype
     (stmt : Statement_Reference;
      iCol : integer)
      return string
   is
      function sqlite3_column_decltype
        (stmt : Stmt_Access;
         iCol : C.int)
         return CS.Chars_Ptr;
      pragma Import(C, sqlite3_column_decltype, "sqlite3_column_decltype");
      -- sqlite3.h:690
      -- const char *sqlite3_column_decltype(sqlite3_stmt *, int i);

   begin

      return To_Ada(sqlite3_column_decltype(stmt.stmt, C.int(iCol)));

   end column_decltype;

   -----------------------
   -- column_decltype16 --
   -----------------------

   function column_decltype16
     (stmt : Statement_Reference;
      iCol : integer)
      return wide_string
   is

      function sqlite3_column_decltype16
        (stmt : Stmt_Access;
         iCol : C.int)
         return CWS.Pointer;
      pragma Import(C, sqlite3_column_decltype16, "sqlite3_column_decltype16");
      -- sqlite3.h:710
      -- const void *sqlite3_column_decltype16(sqlite3_stmt*,int);

   begin

      return To_Ada(sqlite3_column_decltype16(stmt.stmt, C.int(iCol)));

   end column_decltype16;

   ----------------
   -- column_int --
   ----------------

   function column_int
     (stmt : Statement_Reference;
      iCol : integer)
      return integer
   is

      function sqlite3_column_int
        (stmt : Stmt_Access;
         iCol : C.int)
         return C.int;
      pragma Import(C, sqlite3_column_int, "sqlite3_column_int");
      -- sqlite3.h:828
      -- int sqlite3_column_int(sqlite3_stmt*, int iCol);

   begin

      return integer(sqlite3_column_int(stmt.stmt, C.int(iCol)));

   end column_int;

   -----------------
   -- column_text --
   -----------------

   function column_text
     (stmt : Statement_Reference;
      iCol : integer)
      return string
   is

      function sqlite3_column_text
        (stmt : Stmt_Access;
         iCol : C.int)
         return CS.Chars_Ptr;
      pragma Import(C, sqlite3_column_text, "sqlite3_column_text");
      -- sqlite3.h:830
      -- const unsigned char *sqlite3_column_text(sqlite3_stmt*, int iCol);

   begin

      return To_Ada(sqlite3_column_text(stmt.stmt, C.int(iCol)));

   end column_text;

   -------------------
   -- column_text16 --
   -------------------

   function column_text16
     (stmt : Statement_Reference;
      iCol : integer)
      return wide_string
   is

      function sqlite3_column_text16
        (stmt : Stmt_Access;
         iCol : C.int)
         return CWS.Pointer;
      pragma Import(C, sqlite3_column_text16, "sqlite3_column_text16");
      -- sqlite3.h:831
      -- const void *sqlite3_column_text16(sqlite3_stmt*, int iCol);

   begin

      return To_Ada(sqlite3_column_text16(stmt.stmt, C.int(iCol)));

   end column_text16;

   -----------------
   -- column_name --
   -----------------

   function column_name
     (stmt : Statement_Reference;
      iCol : integer)
      return string
   is

      function sqlite3_column_name
        (stmt : Stmt_Access;
         iCol : C.int)
         return CS.Chars_Ptr;
      pragma Import(C, sqlite3_column_name, "sqlite3_column_name");
      -- sqlite3.h:669
      -- const char *sqlite3_column_name(sqlite3_stmt*,int);

   begin

      return To_Ada(sqlite3_column_name(stmt.stmt, C.int(iCol)));

   end column_name;

   -------------------
   -- column_name16 --
   -------------------

   function column_name16
     (stmt : Statement_Reference;
      iCol : integer)
      return wide_string
   is

      function sqlite3_column_name16
        (stmt : Stmt_Access;
         iCol : C.int)
         return CWS.Pointer;
      pragma Import(C, sqlite3_column_name16, "sqlite3_column_name16");
      -- sqlite3.h:670
      -- const void *sqlite3_column_name16(sqlite3_stmt*,int);

   begin

      return To_Ada(sqlite3_column_name16(stmt.stmt, C.int(iCol)));

   end column_name16;

   -----------------
   -- column_type --
   -----------------

   function column_type
     (stmt : Statement_Reference;
      iCol : integer)
      return integer
   is

      function sqlite3_column_type
        (stmt : Stmt_Access;
         iCol : C.int)
         return C.int;
      pragma Import(C, sqlite3_column_type, "sqlite3_column_type");
      -- sqlite3.h:832
      -- int sqlite3_column_type(sqlite3_stmt*, int iCol);

   begin

      return integer(sqlite3_column_type(stmt.stmt, C.int(iCol)));

   end column_type;

   --------------
   -- Complete --
   --------------

   function Complete
     (sql : string)
      return boolean
   is

      function sqlite3_complete
        (sql : C.char_array)
         return C.int;
      pragma Import(C, sqlite3_complete, "sqlite3_complete");
      -- sqlite3.h:233
      -- int sqlite3_complete(const char *sql);

   begin

      if ( integer(sqlite3_complete(C.To_C(sql))) = 0 ) then
         return FALSE;
      else
         return TRUE;
      end if;

   end Complete;

   ----------------
   -- Complete16 --
   ----------------

   function Complete16
     (sql : wide_string)
      return boolean
   is

      function sqlite3_complete16
        (sql : C.wchar_array)
         return C.int;
      pragma Import(C, sqlite3_complete16, "sqlite3_complete16");
      -- sqlite3.h:234
      -- int sqlite3_complete16(const void *sql);

   begin

      if ( integer(sqlite3_complete16(C.To_C(sql))) = 0 ) then
         return FALSE;
      else
         return TRUE;
      end if;

   end Complete16;

   ----------------
   -- data_count --
   ----------------

   function data_count
     (stmt : Statement_Reference)
      return integer
   is

      function sqlite3_data_count
        (stmt : Stmt_Access)
         return C.int;
      pragma Import(C, sqlite3_data_count, "sqlite3_data_count");
      -- sqlite3.h:755
      -- int sqlite3_data_count(sqlite3_stmt *pStmt);

   begin

      return integer(sqlite3_data_count(stmt.stmt));

   end data_count;

   -------------
   -- element --
   -------------

   function Element
     (Self : Table_Reference;
      row  : natural;
      col  : natural)
      return string
   is

      use String_Tables;
      use String_Vectors;

   begin

      return Element(Element(Self.all, row), col);

   end Element;

   ---------------
   -- element16 --
   ---------------

   function Element16
     (Self : Table16_Reference;
      row  : natural;
      col  : natural)
      return wide_string
   is

      use Wide_String_Tables;
      use Wide_String_Vectors;

   begin

      return Element(Element(Self.all, row), col);

   end Element16;

   -------------
   -- errcode --
   -------------

   function errcode
     (Self : Handle)
      return integer
   is

      function sqlite3_errcode
        (db : DB_Access)
         return C.int;
      pragma Import(C, sqlite3_errcode, "sqlite3_errcode");
      -- sqlite3.h:528
      -- int sqlite3_errcode(sqlite3 *db);

   begin

      return integer(sqlite3_errcode(Self.db));

   end errcode;

   ------------
   -- errmsg --
   ------------

   function errmsg
     (self : Handle)
      return string
   is

     function sqlite3_errmsg
       (db : DB_Access)
        return CS.Chars_Ptr;
     pragma Import(C, sqlite3_errmsg, "sqlite3_errmsg");
     -- sqlite3.h:538
     -- const char *sqlite3_errmsg(sqlite3*);

   begin

      return To_Ada(sqlite3_errmsg(Self.db));

   end errmsg;

   --------------
   -- errmsg16 --
   --------------

   function errmsg16
     (self : Handle)
      return wide_string
   is

     function sqlite3_errmsg16
       (db : DB_Access)
        return CWS.Pointer;
     pragma Import(C, sqlite3_errmsg16, "sqlite3_errmsg16");
      -- sqlite3.h:548
      -- const void *sqlite3_errmsg16(sqlite3*);

   begin

      return To_Ada(sqlite3_errmsg16(Self.db));

   end errmsg16;

   ----------
   -- exec --
   ----------

   function Exec
     (Self : Handle;
      sql  : string;
      cb   : Callback)
      return Return_Value
   is

      function Proxy
        (pArg        : Handle;
         argc        : integer;
         argv        : String_Vectors.Vector;
         columnNames : String_Vectors.Vector)
         return Return_Value
      is
      begin
         return cb(argc, argv, columnNames);
      end Proxy;
      pragma Inline(Proxy);

      package Real_Exec
         is new Generic_Exec(Object, Handle);

   begin

      return Real_Exec.Exec(Self, sql, Proxy'Access, null);

   end Exec;

   ------------
   -- exec16 --
   ------------

   function Exec16
     (Self : Handle;
      sql  : wide_string;
      cb   : Callback16)
      return Return_Value
   is

      function Proxy
        (pArg        : Handle;
         argc        : integer;
         argv        : Wide_String_Vectors.Vector;
         columnNames : Wide_String_Vectors.Vector)
         return Return_Value
      is
      begin
         return cb(argc, argv, columnNames);
      end Proxy;
      pragma Inline(Proxy);

      package Real_Exec16
         is new Generic_Exec16(Object, Handle);

   begin

      return Real_Exec16.Exec16(Self, sql, Proxy'Access, null);

   end Exec16;

   ----------
   -- exec --
   ----------

   function Exec
     (Self : Handle;
      sql  : string)
      return Return_Value
   is

      function Nothing
        (argc        : integer;
         argv        : String_Vectors.Vector;
         columnNames : String_Vectors.Vector)
         return Return_Value
      is
      begin
         return SQLITE_OK;
      end Nothing;
      pragma Inline(Nothing);

   begin

      return Exec(Self, sql, Nothing'Unrestricted_Access);

   end Exec;

   ------------
   -- exec16 --
   ------------

   function Exec16
     (Self : Handle;
      sql  : wide_string)
      return Return_Value
   is

      function Nothing
        (argc        : integer;
         argv        : Wide_String_Vectors.Vector;
         columnNames : Wide_String_Vectors.Vector)
         return Return_Value
      is
      begin
         return SQLITE_OK;
      end Nothing;
      pragma Inline(Nothing);

   begin

      return Exec16(Self, sql, Nothing'Unrestricted_Access);

   end Exec16;

   --------------
   -- finalize --
   --------------

   function finalize
     (stmt : Statement_Reference)
      return Return_Value
   is

      function sqlite3_finalize
        (stmt : Stmt_Access)
         return C.int;
      pragma Import(C, sqlite3_finalize, "sqlite3_finalize");
      -- sqlite3.h:848
      -- int sqlite3_finalize(sqlite3_stmt *pStmt);

   begin

      return int_to_ret_val(sqlite3_finalize(stmt.stmt));

   end finalize;

   ------------------
   -- Generic_Exec --
   ------------------

   package body Generic_Exec is

      use String_Vectors;

      function Exec
        (Self    : Handle;
         sql     : string;
         cb      : Callback;
         arg     : Data_Handle)
         return Return_Value
      is

         St  : aliased Statement;
         rSt : Statement_Reference := St'Unrestricted_Access;
         rc  : Return_Value;

         vals      : String_Vectors.Vector;
         cols      : String_Vectors.Vector;
         nCol      : integer;
         cols_done : boolean := FALSE;

      begin

         rc := Prepare(Self, sql, rSt);
         if not (rc = SQLITE_OK) then
            return rc;
         end if;

         nCol := column_count(rSt);

         loop

            rc := Step(rSt);

            if ( rc = SQLITE_ROW ) then

               Clear(vals);

               if not Cols_done then
                  for I in 0 .. (nCol - 1) loop
                     Append(cols, column_name(rSt, I));
                  end loop;

                  Cols_Done := TRUE;
               end if;

               for I in 0 .. (nCol - 1) loop
                  Append(vals, column_text(rSt, I));
               end loop;

               rc := cb(arg, nCol, vals, cols);
               if not ( rc = SQLITE_OK ) then
                  rc := Finalize(rSt);
                  rc := SQLITE_ABORT;
                  exit;
               end if;

            else

               rc := Finalize(rSt);
               exit;

            end if;

         end loop;

         return rc;

      end Exec;

   end Generic_Exec;

   --------------------
   -- Generic_Exec16 --
   --------------------

   package body Generic_Exec16 is

      use Wide_String_Vectors;

      function Exec16
        (Self    : Handle;
         sql     : wide_string;
         cb      : Callback16;
         arg     : Data_Handle)
         return Return_Value
      is

         St  : aliased Statement;
         rSt : Statement_Reference := St'Unrestricted_Access;
         rc  : Return_Value;

         vals      : Wide_String_Vectors.Vector;
         cols      : Wide_String_Vectors.Vector;
         nCol      : integer;
         cols_done : boolean := FALSE;

      begin

         rc := Prepare16(Self, sql, rSt);
         if not (rc = SQLITE_OK) then
            return rc;
         end if;

         nCol := column_count(rSt);

         loop

            rc := Step(rSt);

            if ( rc = SQLITE_ROW ) then

               Clear(vals);

               if not Cols_done then
                  for I in 0 .. (nCol - 1) loop
                     Append(cols, column_name16(rSt, I));
                  end loop;

                  Cols_Done := TRUE;
               end if;

               for I in 0 .. (nCol - 1) loop
                  Append(vals, column_text16(rSt, I));
               end loop;

               rc := cb(arg, nCol, vals, cols);
               if not ( rc = SQLITE_OK ) then
                  rc := Finalize(rSt);
                  rc := SQLITE_ABORT;
                  exit;
               end if;

            else

               rc := Finalize(rSt);
               exit;

            end if;

         end loop;

         return rc;

      end Exec16;

   end Generic_Exec16;

   ---------------
   -- Get_Table --
   ---------------

   function Get_Table
     (Self  : Handle;
      Sql   : string;
      table : Table_Reference)
      return Return_Value
   is

      use String_Tables;
      use String_Vectors;

      package Tables_Exec
         is new Generic_Exec(String_Tables.Vector, Table_Reference);

      function Build_Table
        (pArg        : Table_Reference;
         argc        : integer;
         argv        : String_Vectors.Vector;
         columnNames : String_Vectors.Vector)
         return Return_Value
      is
      begin


         if Is_Empty(pArg.all) then
            Append(pArg.all, columnNames);
         end if;

         Append(pArg.all, argv);

         return SQLITE_OK;

      end Build_Table;

   begin

      Clear(Table.all);
      return Tables_Exec.Exec(Self, sql, Build_Table'Unrestricted_Access, table);

   end Get_Table;

   -----------------
   -- Get_Table16 --
   -----------------

   function Get_Table16
     (Self  : Handle;
      Sql   : wide_string;
      table : Table16_Reference)
      return Return_Value
   is

      use Wide_String_Tables;
      use Wide_String_Vectors;

      package Tables16_Exec16
         is new Generic_Exec16(Wide_String_Tables.Vector, Table16_Reference);

      function Build_Table16
        (pArg        : Table16_Reference;
         argc        : integer;
         argv        : Wide_String_Vectors.Vector;
         columnNames : Wide_String_Vectors.Vector)
         return Return_Value
      is
      begin


         if Is_Empty(pArg.all) then
            Append(pArg.all, columnNames);
         end if;

         Append(pArg.all, argv);

         return SQLITE_OK;

      end Build_Table16;

   begin

      Clear(Table.all);
      return Tables16_Exec16.Exec16(Self,
                                    sql,
                                    Build_Table16'Unrestricted_Access,
                                    table);

   end Get_Table16;

   ---------------
   -- Interrupt --
   ---------------

   procedure Interrupt
     (Self : Handle)
   is

      procedure sqlite3_interrupt
        (db : DB_Access);
      pragma Import(C, sqlite3_interrupt, "sqlite3_interrupt");
      -- sqlite3.h:220
      -- void sqlite3_interrupt(sqlite3*);

   begin

      sqlite3_interrupt(Self.db);

   end Interrupt;

   -----------------------
   -- Last_Insert_Rowid --
   -----------------------

   function Last_Insert_Rowid
     (Self : Handle)
      return uint64
   is

      function sqlite3_last_insert_rowid
        (db : DB_Access)
         return uint64;
      pragma Import(C, sqlite3_last_insert_rowid, "sqlite3_last_insert_rowid");
      -- sqlite3.h:172
      -- sqlite_int64 sqlite3_last_insert_rowid(sqlite3*);

   begin

      return sqlite3_last_insert_rowid(Self.db);

   end Last_Insert_Rowid;

   ----------
   -- open --
   ----------

   function Open
     (Self     : Handle;
      Filename : string)
      return Return_Value
   is

      function sqlite3_open
        (filename  : C.char_array;
         DB_handle : access DB_Access)
         return C.int;
      pragma Import(C, sqlite3_open, "sqlite3_open");
      -- sqlite3.h:504
      -- int sqlite3_open(
      --   const char *filename,   /* Database filename (UTF-8) */
      --   sqlite3 **ppDb          /* OUT: SQLite db handle */
      -- );

   begin
      return int_to_ret_val(sqlite3_open(C.To_C (Filename), Self.db'Access));

   end open;

   ------------
   -- open16 --
   ------------

   function open16
     (Self     : Handle;
      Filename : wide_string)
      return Return_Value
   is

      function sqlite3_open16
        (filename  : C.wchar_array;
         DB_handle : access DB_Access)
         return C.int;
      pragma Import(C, sqlite3_open16, "sqlite3_open16");
      -- sqlite3.h:508
      -- int sqlite3_open16(
      --   const void *filename,   /* Database filename (UTF-16) */
      --   sqlite3 **ppDb          /* OUT: SQLite db handle */
      -- );

   begin

      return int_to_ret_val(sqlite3_open16(To_C(Filename), Self.db'Access));

   end open16;


   -------------
   -- prepare --
   -------------

   function prepare
     (self : Handle;
      sql  : string;
      stmt : Statement_Reference)
      return Return_Value
   is

      function sqlite3_prepare
        (db     : DB_Access;
         zSql   : C.char_array;
         nBytes : C.int;
         ppStmt : access Stmt_Access;
         pzTail : System.Address)
         return C.int;
      pragma Import(C, sqlite3_prepare, "sqlite3_prepare");
      -- sqlite3.h:581
      -- int sqlite3_prepare(
      --   sqlite3 *db,            /* Database handle */
      --   const char *zSql,       /* SQL statement, UTF-8 encoded */
      --   int nBytes,             /* Length of zSql in bytes. */
      --   sqlite3_stmt **ppStmt,  /* OUT: Statement handle */
      --   const char **pzTail     /* OUT: Pointer to unused portion of zSql */
      -- );

   begin

      return int_to_ret_val(sqlite3_prepare(Self.db,
                                            C.To_C(sql),
                                            C.int(-1),
                                            stmt.stmt'Access,
                                            System.Null_Address)
                    );

   end prepare;

   ---------------
   -- prepare16 --
   ---------------

   function prepare16
     (self : Handle;
      sql  : wide_string;
      stmt : Statement_Reference)
      return Return_Value
   is

      function sqlite3_prepare16
        (db     : DB_Access;
         zSql   : C.wchar_array;
         nBytes : C.int;
         ppStmt : access Stmt_Access;
         pzTail : System.Address)
         return C.int;
      pragma Import(C, sqlite3_prepare16, "sqlite3_prepare16");
      -- sqlite3.h:588
      -- int sqlite3_prepare16(
      --   sqlite3 *db,            /* Database handle */
      --   const void *zSql,       /* SQL statement, UTF-16 encoded */
      --   int nBytes,             /* Length of zSql in bytes. */
      --   sqlite3_stmt **ppStmt,  /* OUT: Statement handle */
      --   const void **pzTail     /* OUT: Pointer to unused portion of zSql */
      -- );

   begin

      return int_to_ret_val(sqlite3_prepare16(Self.db,
                                              To_C(sql),
                                              C.int(-1),
                                              stmt.stmt'Access,
                                              System.Null_Address)
                    );

   end prepare16;

   -----------
   -- Reset --
   -----------

   function reset
     (stmt : Statement_Reference)
      return Return_Value
   is

      function sqlite3_reset
        (stmt : Stmt_Access)
         return C.int;
      pragma Import(C, sqlite3_reset, "sqlite3_reset");
      -- sqlite3.h:857
      -- int sqlite3_reset(sqlite3_stmt *pStmt);

   begin

      return int_to_ret_val(sqlite3_reset(stmt.stmt));

   end reset;

   ---------------
   -- Row_Count --
   ---------------

   function Row_Count
     (Self : Table_Reference)
      return natural
   is

      use String_Tables;

   begin

      return natural(Length(Self.all));

   end Row_Count;

   ----------
   -- Step --
   ----------

   function step
     (stmt : Statement_Reference)
      return Return_Value
   is

      function sqlite3_step
        (stmt : Stmt_Access)
         return C.int;
      pragma Import(C, sqlite3_step, "sqlite3_step");
      -- sqlite3.h:744
      -- int sqlite3_step(sqlite3_stmt*);

   begin

      return int_to_ret_val(sqlite3_step(stmt.stmt));

   end step;

   -------------
   -- Total_Changes --
   -------------

   function Total_Changes
     (Self : Handle)
      return integer
   is

   	function sqlite3_total_changes
         (db : DB_Access)
          return C.int;
      pragma Import(C, sqlite3_total_changes, "sqlite3_total_changes");
      -- sqlite3.h:212
      -- int sqlite3_total_changes(sqlite3*);

   begin

      return integer(sqlite3_total_changes(Self.db));

   end Total_Changes;

end GNU.DB.SQLite3;
