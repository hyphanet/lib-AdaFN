with Ada.Unchecked_Deallocation;
with Agpl.Text_Io; use Agpl.Text_Io;

package body Agpl.Generic_Dense_Matrix is

   use Ada.Finalization;

   ------------
   -- Create --
   ------------

   function Create
     (Last_Row,
      Last_Col  : Index_Type;
      First_Row,
      First_Col : Index_Type := Index_Type'First)
      return Matrix
   is
   begin
      return (Controlled with new Inner_Matrix (First_Row .. Last_Row,
                                                First_Col .. Last_Col));
   end Create;

   ------------
   -- Create --
   ------------

   function Create (Last_Row,
                    Last_Col  : Index_Type;
                    Default   : Cell_Type;
                    First_Row,
                    First_Col : Index_Type := Index_Type'First)
                    return      Matrix
   is
      Result : constant Matrix := Create (Last_Row,
                                          Last_Col,
                                          First_Row,
                                          First_Col);
   begin
      for I in Result.Ptr'Range (1) loop
         for J in Result.Ptr'Range (2) loop
            Result.Ptr (I, J) := Default;
         end loop;
      end loop;

      return Result;
   end Create;

   ---------------
   -- First_Row --
   ---------------

   function First_Row (This : in Matrix) return Index_Type is
   begin
      return This.Ptr.all'First (1);
   end First_Row;

   --------------
   -- Last_Row --
   --------------

   function Last_Row (This : in Matrix) return Index_Type is
   begin
      return This.Ptr.all'Last (1);
   end Last_Row;

   ---------------
   -- First_Col --
   ---------------

   function First_Col (This : in Matrix) return Index_Type is
   begin
      return This.Ptr.all'First (2);
   end First_Col;

   --------------
   -- Last_Col --
   --------------

   function Last_Col (This : in Matrix) return Index_Type is
   begin
      return This.Ptr.all'Last (2);
   end Last_Col;

   ----------
   -- Rows --
   ----------

   function Rows (This : in Matrix) return Index_Type is
   begin
      return This.Ptr'Length (1);
   end Rows;

   ----------
   -- Cols --
   ----------

   function Cols (This : in Matrix) return Index_Type is
   begin
      return This.Ptr'Length (2);
   end Cols;

   ---------
   -- Get --
   ---------

   function Get
     (This     : in Matrix;
      Row, Col : in Index_Type)
      return Cell_Type
   is
   begin
      return This.Ptr (Row, Col);
   exception
      when others =>
         Put_Line ("Out of bounds:" & Row'Img & Col'Img & " -- " &
                   This.Ptr'First'Img & This.Ptr'Last'Img &
                   This.Ptr'First (2)'Img & This.Ptr'Last (2)'Img);
         raise;
   end Get;

   ---------
   -- Ref --
   ---------

   function Ref
     (This     : in Matrix;
      Row, Col : in Index_Type)
      return access Cell_Type
   is
   begin
      return This.Ptr (Row, Col)'Access;
   end Ref;

   ---------
   -- Set --
   ---------

   procedure Set
     (This : in out Matrix;
      Row,
      Col  : in     Index_Type;
      Data : in     Cell_Type)
   is
   begin
      This.Ptr (Row, Col) := Data;
   end Set;

   procedure Free is new Ada.Unchecked_Deallocation (Inner_Matrix, Inner_Access);

   ------------
   -- Adjust --
   ------------

   procedure Adjust (This : in out Matrix) is
   begin
      This.Ptr := new Inner_Matrix'(This.Ptr.all);
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Matrix) is
   begin
      Free (This.Ptr);
   end Finalize;

   ----------
   -- Read --
   ----------

   procedure Read (Stream : not null access Root_Stream_Type'Class;
                   This   : out Matrix)
   is
      Fr : constant Index_Type := Index_Type'Input (Stream);
      Lr : constant Index_Type := Index_Type'Input (Stream);
      Fc : constant Index_Type := Index_Type'Input (Stream);
      Lc : constant Index_Type := Index_Type'Input (Stream);
   begin
      This := Create (First_Row => Fr,
                      Last_Row  => Lr,
                      First_Col => Fc,
                      Last_Col  => Lc);
      Inner_Matrix'Read (Stream, This.Ptr.all);
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write (Stream : not null access Root_Stream_Type'Class;
                    This   :     Matrix)
   is
   begin
      Index_Type'Output (Stream, This.Ptr'First (1));
      Index_Type'Output (Stream, This.Ptr'Last (1));
      Index_Type'Output (Stream, This.Ptr'First (2));
      Index_Type'Output (Stream, This.Ptr'Last (2));
      Inner_Matrix'Write (Stream, This.Ptr.all);
   end Write;

end Agpl.Generic_Dense_Matrix;
