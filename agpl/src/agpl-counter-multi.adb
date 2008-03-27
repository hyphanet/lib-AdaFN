 

with Ada.Unchecked_Deallocation;

package body Agpl.Counter.Multi is

   use Counter_Map;

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   protected body Safe_Object is
      --  Creates it if doesn't exists with value Increment
      procedure Add (Key : in String; Increment : in Integer := 1) is
         I : constant Cursor := Find (Values, Key);
         C : Counter.Object_Access;
      begin
         if I = No_Element then
            C := new Counter.Object;
            Insert (Values, Key, C);
         else
            C := Element (I);
         end if;
         C.Add (Increment);
      end Add;

      procedure Reset (Key : in String; Val     : in Integer := 0) is
         I : constant Cursor := Find (Values, Key);
         C : Counter.Object_Access;
      begin
         if I = No_Element then
            C := new Counter.Object;
            Insert (Values, Key, C);
         else
            C := Element (I);
         end if;
         C.Reset (Val);
      end Reset;

      function  Val (Key : in String) return Integer is
      begin
         return Element (Find (Values, Key)).Val;
      end Val;

      function  Max_Key return String is
         Max : Integer := Integer'First;
         Pos : Cursor := No_Element;
         I   : Cursor := First (Values);
      begin
         while I /= No_Element loop
            if Element (I).Val >= Max then
               Pos := I;
               Max := Element (I).Val;
            end if;
            Next (I);
         end loop;
         if Pos = No_Element then
            return "";
         else
            return Key (Pos);
         end if;
      end Max_Key;

      procedure Destroy is
         procedure Free is new Unchecked_Deallocation (
            Counter.Object, Counter.Object_Access);
         I   : Cursor := First (Values);
         Aux : Counter.Object_Access;
      begin
         while I /= No_Element loop
            Aux := Element (I);
            Free (Aux);
            Next (I);
         end loop;
      end Destroy;
   end Safe_Object;

   ------------------------------------------------------------------------
   -- Finalize                                                           --
   ------------------------------------------------------------------------
   procedure Finalize (This : in out Object) is
   begin
      This.Safe.Destroy;
   end Finalize;

   ------------------------------------------------------------------------
   -- Add                                                                --
   ------------------------------------------------------------------------
   --  Creates it if doesn't exists with value Increment
   procedure Add (This : in out Object; Key : in String; Increment : in Integer := 1) is
   begin
      This.Safe.Add (Key, Increment);
   end Add;

   ------------------------------------------------------------------------
   -- Reset                                                              --
   ------------------------------------------------------------------------
   procedure Reset (This : in out Object; Key : in String; Val : in Integer := 0) is
   begin
      This.Safe.Reset (Key, Val);
   end Reset;

   ------------------------------------------------------------------------
   -- Val                                                                --
   ------------------------------------------------------------------------
   function  Val (This : in Object; Key : in String) return Integer is
   begin
      return This.Safe.Val (Key);
   end Val;

   ------------------------------------------------------------------------
   -- Max_Key                                                            --
   ------------------------------------------------------------------------
   function  Max_Key (This : in Object) return String is
   begin
      return This.Safe.Max_Key;
   end Max_Key;

end Agpl.Counter.Multi;
