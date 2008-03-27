package AdaFN is

   pragma Pure;

   Log_Section : constant String := "adafn";

   type Persistences is (Connection, Reboot, Forever);

   type Return_Types is (Direct, Disk, None);

   type Priorities is range 0 .. 7;
   --  Highest .. Lowest

   type Key_Size is new Natural;

   Default_Priority : constant Priorities := 2;
   Default_Timeout  : constant Duration   := 5.0 * 60.0;

end AdaFN;
