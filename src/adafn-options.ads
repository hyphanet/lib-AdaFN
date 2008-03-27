with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

package Adafn.Options is

   --  pragma Preelaborate;

   type Verbosities is range 0 .. 513;
   Verbose_Silent   : constant Verbosities := 0;
   Verbose_Progress : constant Verbosities := 1;
   Verbose_Compress : constant Verbosities := 512;
   Verbose_All      : constant Verbosities :=
      Verbose_Progress +
      Verbose_Compress;

   type For_Request is record
      Max_Size         : Natural      := Natural'Last;
      Priority         : Priorities   := Default_Priority;
      Persistence      : Persistences := Connection;
      Global_Queue     : Boolean      := False;
      Return_Type      : Return_Types := Direct;
      Max_Retries      : Natural      := 3;
      Verbose          : Verbosities  := Verbose_Silent;
      From_Datastore   : Boolean      := False;
      Timeout          : Duration     := Default_Timeout;
      Follow_Redirects : Natural      := Natural'Last; -- Amount to follow
   end record;

   type For_Insert is record
      Priority            : Priorities   := Default_Priority;
      Persistence         : Persistences := Forever;
      Global_Queue        : Boolean      := True;
      Max_Retries         : Natural      := 3;
      Verbose             : Verbosities  := Verbose_Silent;
      Compress            : Boolean      := True;
      Default_File        : Ustring      := +"index.html";
      Allow_If_Unreadable : Boolean      := False;
      Timeout             : Duration     := Duration'Last;
      Preferred_Send      : Return_Types := Direct; -- Works with remote nodes
   end record;

   Default_For_Request : constant For_Request := (others => <>);
   Default_For_Insert  : constant For_Insert  := (others => <>);

end Adafn.Options;
