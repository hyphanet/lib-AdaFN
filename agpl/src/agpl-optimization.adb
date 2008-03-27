with Agpl.Conversions;

package body Agpl.Optimization is

   function S is new Conversions.To_Str (Cost);

   -----------
   -- Image --
   -----------

   function Image (C : in Cost; Decimals : in Natural := 2) return String is
   begin
      return S (C, Decimals);
   end Image;

end Agpl.Optimization;
