with Agpl.Interfaces.C.Arrays;

with Interfaces.C;

package Agpl.Interfaces.C.Types is

   --  Types for interfacing with C/C++ commonly used across AGPL

   pragma Pure;

   package Ic renames Standard.Interfaces.C;

   type Double           is new Ic.Double;
   type Int              is new Ic.Int;

   package Int_Arrays    is new Arrays (Int);
   package Double_Arrays is new Arrays (Double);

end Agpl.Interfaces.C.Types;
