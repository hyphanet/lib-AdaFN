 

package body Agpl.Bandwidth_Throttle is

   use Ada.Streams;

   Safe_Maximum : constant := (Stream_Element_Count'Last - 1) / 2;

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   protected body Object is

      procedure Refresh;
      pragma Inline (Refresh);

      ---------------
      -- Available --
      ---------------
      --  Gives the available bandwidth at this moment (real + extra):
      procedure Available (Bandwidth : out Stream_Element_Count) is
      begin
         Refresh;
         begin
            Bandwidth := Remanent + Unused;
         exception
            when Constraint_Error =>
               Bandwidth := Stream_Element_Count'Last;
         end;
      end Available;

      ------------
      -- Commit --
      ------------
      --  Issue a bandwidth petition. Awarded can be less that solicited.
      --  Extra flag requests bandwidth from unused past cycles.
      procedure Commit (
         Desired : in     Stream_Element_Count;
         Awarded :    out Stream_Element_Count;
         Extra   : in     Boolean := False) is
      begin
         Refresh;
         if Extra then
            Awarded  := Stream_Element_Count'Min (
               Desired, Stream_Element_Count'Min (Unused, Safe_Maximum));
            Unused   := Unused - Awarded;
         else
            Awarded  := Stream_Element_Count'Min (
               Desired, Stream_Element_Count'Min (Remanent, Safe_Maximum));
            Remanent := Remanent - Awarded;
         end if;
      end Commit;

      -------------
      -- Refresh --
      -------------
      --  Recomputes if necessary the BW available
      procedure Refresh is
         use Ada.Calendar;
         Elapsed : constant Duration := Clock - Last_Req;
      begin
         --  Check for too many time elapsed:
         if Elapsed >= Gap then
            --  Keep unused from past cycles:
            Unused := Remanent;
            --  Update remanent:
            begin
               Remanent := Stream_Element_Count (Float'Floor (Float (Bandwidth) * Float (Gap)));
            exception
               when Constraint_Error =>
                  Remanent := Stream_Element_Count'Last;
            end;
            --  Update clock:
            declare
               Gaps : constant Natural :=
                  Natural (Float'Floor (Float (Elapsed) / Float (Gap)));
            begin
               Last_Req := Last_Req + Gap * Duration (Gaps);
            end;
         end if;
      end Refresh;

   end Object;

end Agpl.Bandwidth_Throttle;
