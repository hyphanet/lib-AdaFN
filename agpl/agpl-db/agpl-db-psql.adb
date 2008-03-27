with Agpl.Strings;
with Agpl.Trace; use Agpl.Trace;

package body Agpl.Db.Psql is

   function Connect (Db   : String;
                     User : String;
                     Pass : String;
                     Host : String  := "127.0.0.1";
                     Port : Natural := 5432;
                     Ssl  : Boolean := True) return Database_Access
   is
      use Agpl.Strings;

      function Modes (B : Boolean) return String is
      begin
         if B then return "enable"; else return "disable"; end if;
      end Modes;

      Parm : aliased String :=
               "host=" & Host &
               " port=" & Trim (Port'Img) &
               " dbname=" & Db &
               " user=" & User &
               " password=" & Pass &
               " sslmode=" & Modes (Ssl);

      This : constant Database_Access := new Database (Parm'Unchecked_Access);
   begin
      return This;
   end Connect;

   -----------------
   -- Check_Query --
   -----------------

   procedure Check_Query (R : Result; Db : Database) is
   begin
      case Status (R) is
         when Pgres_Empty_Query | Pgres_Bad_Response | Pgres_Fatal_Error | Pgres_Nonfatal_Error =>
            Log ("Agpl.Db.Psql.Check_Query: " & "Error: " & Error (R) & "; Status: " & Status (R), Error);
            Rollback (Db);
            raise Database_Error with "Error: " & Error (R) & "; Status: " & Status (R);
         when others =>
            null;
      end case;
   end Check_Query;

   -----------
   -- Query --
   -----------

   procedure Query (R : out Result; Db : Database; Q : String) is
   begin
      --  Log ("QUERY: " & Q, Always);
      Execute (R, Db, Q);
      Check_Query (R, Db);
   exception
      when E : others =>
         Log ("Agpl.Db.Psql.Query [Exception for query]: " & Q, Error);
         Log ("Query: " & Report (E), Error);
         Log ("Error: " & Error (R) & "; Status: " & Status (R), Error);
   end Query;

   -----------
   -- Query --
   -----------

   function Query (Db : Database; Sql : String) return String is
      R : Result;
   begin
      Query (R, Db, Sql);
      return Value (R, 0, 0);
   end Query;

   -----------------------
   -- Begin_Transaction --
   -----------------------

   procedure Begin_Transaction (Db : Database) is
      R : Result;
   begin
      Query (R, Db, "begin");
   end Begin_Transaction;

   ------------------------
   -- Commit_Transaction --
   ------------------------

   procedure Commit_Transaction (Db : Database) is
      R : Result;
   begin
      Query (R, Db, "commit");
   end Commit_Transaction;

   --------------------------
   -- Rollback_Transaction --
   --------------------------

   procedure Rollback_Transaction (Db : Database) is
      Roll : Result;
   begin
      Execute (Roll, Db, "rollback");
      --  Execute instead of query: either the rollback works or we're doomed anyway
   end Rollback_Transaction;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Transaction) is
   begin
      Begin_Transaction (This.Db.all);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize   (This : in out Transaction) is
   begin
      Commit (This.Db.all);
   end Finalize;

   ------------
   -- Escape --
   ------------

   function Escape (This : String) return String is
   begin
      return Strings.Replace (This, "'", "''");
   end Escape;

   -------------
   -- Unescape --
   -------------

   function Unescape (This : String) return String is
   begin
      return Strings.Replace (This, "''", "'");
   end Unescape;

   -----------------------
   -- Load_Table_Column --
   -----------------------

   function Load_Table_Column (Db : Database; Table : String; Column : Positive)
                               return Agpl.Containers.String_Sets.Set
   is
      R    : Gnu.Db.Postgresql.Result;
      S    : Containers.String_Sets.Set;
   begin
      Query
        (R, Db,
         "select * from """ & Table & """");

      for I in 0 .. Tuple_Count (R) - 1 loop
         S.Include (Value (R, I, Gnu.Db.Postgresql.Field_Index (Column - 1)));
      end loop;
      return S;
   end Load_Table_Column;

   ----------------
   -- Bool_Value --
   ----------------

   function Bool_Value (Str : String) return Boolean is
   begin
      if Str (Str'First) = 't' or Str (Str'First) = 'T' then
         return True;
      else
         return False;
      end if;
   end Bool_Value;

end Agpl.Db.Psql;
