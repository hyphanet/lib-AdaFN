with Adafn.Key;
with Adafn.Metadata;

with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

package Adafn.Result is

   type Object is tagged record
      Uri  : Key.Object;
      Data : Ustring;
      Meta : Metadata.Object;
   end record;

end Adafn.Result;
