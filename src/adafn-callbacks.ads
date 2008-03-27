with Adafn.Transaction;
use Adafn;

package Adafn.Callbacks is

   function Silent return Boolean;

   procedure Cb (This    :     Transaction.Object;
                 Action  : out Transaction.Actions);

end Adafn.Callbacks;
