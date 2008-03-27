 

--  Type for generating a simple graph
--  Data values are floats, vertical scale is automatic, horizontal is fixed,
--  multiple series are allowed. Values in multiple series should be normali-
--  zed against a common scale.

--  Based in the Agpl.Average_Queue.Timed package, this allows to forget
--  about timing concerns. You push data into the graph and it will automa-
--  gically mix/average the data. The samples are added regularly as speci-
--  fied for the timed average queue.

with Ada.Unchecked_Deallocation;

with Text_IO;

package body Agpl.Graph.Timed is

   ------------------------------------------------------------------------
   -- Add_sample                                                         --
   ------------------------------------------------------------------------
   procedure Add_sample (This : in out Object; Serie : in Natural; Sample : in Float) is
      Gap_Change : Boolean;
      Empty_Gaps : Natural;
      Average    : Float;
   begin
      Avg.Extended_Push (This.Avgs (Serie).all, Sample, Gap_Change, Empty_Gaps);
      Text_IO.Put_Line (Gap_Change'Img);
      if Gap_Change then
         Avg.Average (This.Avgs (Serie).all, Average);
         Text_IO.Put_Line (Average'Img);
         Graph.Add_Sample (Graph.Object (This), Serie, Average);
         for I in 1 .. Empty_Gaps loop
            Text_IO.Put_Line ("Empty gap");
            Graph.Add_Sample (Graph.Object (This), Serie, 0.0);
         end loop;
      end if;
   end Add_Sample;

   ------------------------------------------------------------------------
   -- Finalization                                                       --
   ------------------------------------------------------------------------
   procedure Initialize (This : in out Controller) is
   begin
      for I in This.Parent.Avgs'Range loop
         This.Parent.Avgs (I) :=
            new Avg.Object (
               Slots         => This.Parent.Sample_Averages,
               Slot_Duration => This.Parent.Sample_Duration);
      end loop;
   end Initialize;

   procedure Finalize   (This : in out Controller) is
      procedure Free is new Unchecked_Deallocation (Avg.Object, Avg.Object_Access);
   begin
      for I in This.Parent.Avgs'Range loop
         Free (This.Parent.Avgs (I));
      end loop;
   end Finalize;

end Agpl.Graph.Timed;
