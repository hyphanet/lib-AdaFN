package body Agpl.Containers.Utils is

   ------------
   -- To_Set --
   ------------

   function To_Set (X : Integer_Array) return Integer_Sets.Set is
      Y : Integer_Sets.Set;
   begin
      for I in X'Range loop
         Y.Include (X (I));
      end loop;
      return Y;
   end To_Set;

end Agpl.Containers.Utils;
