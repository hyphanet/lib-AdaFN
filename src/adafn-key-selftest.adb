with Adafn.Key; pragma Elaborate_All (Adafn.Key);

with Agpl.Trace; use Agpl.Trace;

package body Adafn.Key.Selftest is

   Keys : constant Ustring_Array :=
            (+"CHK@UzheFAwyic8ewgeGfYMPp-LJYDb0JVhH~Hyj3OxBD0M,ecX2efnXZmXI-LPR7tl4K1C-KAScoiQKTIFZ6Ohtbng,AAICAAA",
             +"CHK@UzheFAwyic8ewgeGfYMPp-LJYDb0JVhH~Hyj3OxBD0M,ecX2efnXZmXI-LPR7tl4K1C-KAScoiQKTIFZ6Ohtbng,AAICAAA/",
             +"CHK@UzheFAwyic8ewgeGfYMPp-LJYDb0JVhH~Hyj3OxBD0M,ecX2efnXZmXI-LPR7tl4K1C-KAScoiQKTIFZ6Ohtbng,AAICAAA/blah.txt",
             +"USK@3csXsCCJHQGnOkoT-bIDk6rTq1Z3eAKOElUlW-dtI2o,4M-s6pt95sFNx6GwTQw-JObrhDmsqk6DNDGjybnhapM,AQACAAE/test/1",
             +"USK@3csXsCCJHQGnOkoT-bIDk6rTq1Z3eAKOElUlW-dtI2o,4M-s6pt95sFNx6GwTQw-JObrhDmsqk6DNDGjybnhapM,AQACAAE/test/1/",
             +"SSK@3csXsCCJHQGnOkoT-bIDk6rTq1Z3eAKOElUlW-dtI2o,4M-s6pt95sFNx6GwTQw-JObrhDmsqk6DNDGjybnhapM,AQACAAE/test-1",
             +"SSK@3csXsCCJHQGnOkoT-bIDk6rTq1Z3eAKOElUlW-dtI2o,4M-s6pt95sFNx6GwTQw-JObrhDmsqk6DNDGjybnhapM,AQACAAE/test-1/",
             +"KSK@cualquiercosa.txt",
             +"USK@3csXsCCJHQGnOkoT-bIDk6rTq1Z3eAKOElUlW-dtI2o,4M-s6pt95sFNx6GwTQw-JObrhDmsqk6DNDGjybnhapM,AQACAAE/test/1/file.txt"
            );

   Parents : constant Ustring_Array :=
               (+"/",
                +"/",
                +"/",
                +"/test/",
                +"/test/1/",
                +"/",
                +"/test-1/",
                +"/",
                +"/test/1/"
               );

   Files : constant Ustring_Array :=
             (+"",
              +"",
              +"blah.txt",
              +"",
              +"",
              +"",
              +"",
              +"cualquiercosa.txt",
              +"file.txt"
             );

   Bares : constant array (Files'Range) of Boolean :=
             (True,
              False,
              False,
              True,
              False,
              True,
              False,
              False,
              False);


   procedure Check (Level : Agpl.Trace.Levels := Never) is
      Checking : Natural;
   begin
      --  Check Image/Value
      for I in Keys'Range loop
         Checking := I;
         if Image (Value (+Keys (I))) /= +Keys (I) then
            Log (Checking'Img & ": " & Image (Value (+Keys (I))), Level);
            raise Program_Error with "Adafn.Key.Selftest 1 failed";
         end if;

         declare
            Kind : Kinds;
            Cryp : Ustring;
            Path : Ustring;
            Revi : Integer;
         begin
            Split (Value (+Keys (I)), Kind, Cryp, Path, Revi);
            Log (Checking'Img & ": " &
                 "Cryp: " & (+Cryp) &
                 " Path: " & (+Path) &
                 " Revi:" & (Revi'Img), Level);
            if Image (Assemble (Kind, +Cryp, +Path, Revi)) /= +Keys (I) then
               raise Program_Error with "Adafn.Key.Selftest 2 failed";
            end if;
            begin
               if Kind = Usk and then Extract_Revision (+Keys (I)) /= Revi then
                  raise Program_Error with "Adafn.Key.Selftest 3 failed";
               end if;
            exception
               when others =>
                  raise Program_Error with "Adafn.Key.Selftest 4 failed";
            end;
         end;
      end loop;

      --  Check parenting and file and bare extraction
      for I in Keys'Range loop
         if Parent_Path (Value (+Keys (I))) /= +Parents (I) then
            raise Program_Error with "Parenting failed for" & I'Img & ": " &
            Parent_Path (Value (+Keys (I))) & " /= " & (+Parents (I));
         end if;
         if File (Value (+Keys (I))) /= +Files (I) then
            raise Program_Error with "File failed for" & I'Img & ": " &
            File (Value (+Keys (I))) & " /= " & (+Files (I));
         end if;
         if Is_Bare (Value (+Keys (I))) /= Bares (I) then
            raise Program_Error with "Is_Bare failed for" & I'Img & ": " &
            Is_Bare (Value (+Keys (I)))'Img & " /= " & Bares (I)'Img;
         end if;
      end loop;

   exception
      when E : others =>
         Log ("Adafn.Key.Selftest: [Checking" & Checking'Img & "] " & Report (E),
              Error);
         raise;
   end Check;
begin
   Check;
exception
   when others =>
      Check (Agpl.Trace.Warning);
      raise;
end Adafn.Key.Selftest;
