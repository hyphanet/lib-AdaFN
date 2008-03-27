package body Agpl.Trace.Root is

   --------------
   -- Decorate --
   --------------

   function Decorate (This    : in Object;
                      Text    : in String;
                      Level   : in Levels;
                      Section : in String) return String
   is
   begin
      if This.Decor /= null then
         return This.Decor (Text, Level, Section);
      else
         return Text;
      end if;
   end Decorate;

   --------------
   -- Must_Log --
   --------------

   function Must_Log (This    : in Object;
                      Level   : in Levels;
                      Section : in String) return Boolean
   is
   begin
      return
        This.Active and then
        Level >= This.Level and then
          (Level >= Warning or else
           Section = "" or else
           This.Sections.Contains (Section));
   end Must_Log;

   --------------------
   -- Enable_Section --
   --------------------

   procedure Enable_Section (This    : in out Object;
                             Section : in     String;
                             Enabled : in     Boolean := True) is
   begin
      if Enabled then
         This.Sections.Include (Section);
      else
         This.Sections.Exclude (Section);
      end if;
   end Enable_Section;

   ----------------
   -- Set_Active --
   ----------------

   procedure Set_Active (This : in out Object; Active : in Boolean := True) is
   begin
      This.Active := Active;
   end Set_Active;

   -------------------
   -- Set_Decorator --
   -------------------

   procedure Set_Decorator (This : in out Object; Decor : in Decorator) is
   begin
      This.Decor := Decor;
   end Set_Decorator;

   ---------------
   -- Set_Level --
   ---------------

   procedure Set_Level (This : in out Object; Level : in All_Levels) is
   begin
      This.Level := Level;
   end Set_Level;

end Agpl.Trace.Root;
