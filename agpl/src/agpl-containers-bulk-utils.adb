--  with Agpl.Trace; use Agpl.Trace;

package body Agpl.Containers.Bulk.Utils is

   -----------------
   -- Concatenate --
   -----------------

   procedure Concatenate (Dest : in out Lists.List; Src : in Lists.List) is

      use Lists;

      procedure Append (X : in Cursor) is
      begin
         Dest.Append (Element (X));
      end Append;

   begin
      Iterate (Src, Append'Access);
   end Concatenate;

   ---------
   -- "+" --
   ---------

   function "+" (L, R : Lists.List) return Lists.List is
      Result : Lists.List := L;
   begin
      Append (Result, R);
      return Result;
   end "+";

   -------------
   -- To_List --
   -------------

   function To_List (Src : in Vectors.Vector) return Lists.List is
      Dst : Lists.List;

      procedure Add (I : Vectors.Cursor) is
      begin
         Dst.Append (Vectors.Element (I));
      end Add;
   begin
      Src.Iterate (Add'Access);
      return Dst;
   end To_List;

   function To_List (Src :    Element_Type) return Lists.List is
      L : Lists.List;
   begin
      L.Append (Src);
      return L;
   end To_List;

   ------------
   -- To_Map --
   ------------

   function To_Map (Key : Key_Type; Src : Element_Type) return Maps.Map is
      Result : Maps.Map;
   begin
      Result.Insert (Key, Src);
      return Result;
   end To_Map;

   ------------
   -- To_Map --
   ------------

   function To_Map (Src : Lists.List;
                    Key : access function (E : Element_Type) return Key_Type)
                    return Maps.Map
   is
      Dst : Maps.Map;

      procedure Add (I : Lists.Cursor) is
      begin
         Dst.Insert (Key (Lists.Element (I)), Lists.Element (I));
      end Add;
   begin
      Src.Iterate (Add'Access);
      return Dst;
   end To_Map;

   ---------------
   -- To_Vector --
   ---------------

   function To_Vector (Src : in Lists.List) return Vectors.Vector is
      Dst : Vectors.Vector;

      procedure Add (I : Lists.Cursor) is
      begin
         Dst.Append (Lists.Element (I));
      end Add;
   begin
      Src.Iterate (Add'Access);
      return Dst;
   end To_Vector;

   -------------------
   -- Reverse_Slice --
   -------------------

   function Reverse_Slice (Src : in Vectors.Vector;
                           Ini : in Index_Type;
                           Fin : in Index_Type) return Vectors.Vector
   is
      Dst : Vectors.Vector := Vectors.To_Vector (Src.Length);
   begin
      for I in Src.First_Index .. Ini - 1 loop
         Dst.Replace_Element (I, Src.Element (I));
      end loop;
      for I in Fin + 1 .. Src.Last_Index loop
         Dst.Replace_Element (I, Src.Element (I));
      end loop;

      for I in Ini .. Fin loop
         Dst.Replace_Element (Fin - (I - Ini), Src.Element (I));
      end loop;

      return Dst;
   end Reverse_Slice;

   ---------
   -- "-" --
   ---------

   function "-" (L, R : Lists.List) return Lists.List is
      X : Lists.List := L;

      procedure Check (I : Lists.Cursor) is
         Target : Lists.Cursor := X.Find (Lists.Element (I));
      begin
--         Log ("Substracting 1", Always);
         loop
--            Log ("Substracting 1.1", Always);
            exit when not Lists.Has_Element (Target);
--            Log ("Substracting 1.2", Always);
            Target := X.Find (Lists.Element (I));
--            Log ("Substracting 1.3", Always);
            X.Delete (Target);
--            Log ("Substracting 1.4", Always);
         end loop;
      end Check;
   begin
--      Log ("Substracting", always);
      R.Iterate (Check'Access);

      return X;
   end "-";

   ---------
   -- "-" --
   ---------

   function "-" (L : Lists.List; R : Element_Type) return Lists.List is
   begin
      return L - To_List (R);
   end "-";

   -----------
   -- Slice --
   -----------

   function Slice (L     : Lists.List;
                   First : Positive;
                   Last  : Natural) return Lists.List
   is
      Result : Lists.List;
      use Lists;
      I      : Cursor  := L.First;
      Reach  : Natural := First;
      Copy   : Natural := Last - First + 1;
   begin
      if Last >= First then
         while Reach > 1 loop
            Reach := Reach - 1;
            Next (I);
         end loop;
         while Copy >= 1 loop
            Result.Append (Element (I));
            Copy := Copy - 1;
            Next (I);
         end loop;
      end if;

      return Result;
   end Slice;

   ----------------
   -- To_Key_Set --
   ----------------

   function To_Key_Set (Src                 : Lists.List;
                        Allow_Repeated_Keys : Boolean := False)
                        return Key_Sets.Set
   is
      S : Key_Sets.Set;
      procedure Add (I : Lists.Cursor) is
      begin
         if Allow_Repeated_Keys then
            S.Include (Key (Lists.Element (I)));
         else
            S.Insert (Key (Lists.Element (I)));
         end if;
      end Add;
   begin
      Src.Iterate (Add'Access);
      return S;
   end To_Key_Set;

   ---------------
   -- Intersect --
   ---------------

   function Intersect (L, R : Lists.List) return Lists.List is
      Keys : constant Key_Sets.Set := To_Key_Set (R);
      I    :          Lists.Cursor := L.First;
      Res  :          Lists.List;
   begin
      while Lists.Has_Element (I) loop
         if Keys.Contains (Key (Lists.Element (I))) then
            Res.Append (Lists.Element (I));
         end if;
         Lists.Next (I);
      end loop;
      return Res;
   end Intersect;

   ------------
   -- Delete --
   ------------

   procedure Delete (L : in out Lists.List;
                     K :        Key_Type;
                     Fail_If_Missing : Boolean := True)
   is
      use Lists;
      I : Cursor := L.First;
   begin
      while Has_Element (I) loop
         if Key (Element (I)) = K then
            L.Delete (I);
            return;
         end if;
         Next (I);
      end loop;

      if Fail_If_Missing then
         raise Constraint_Error with "Key not found";
      end if;
   end Delete;

   ----------
   -- Find --
   ----------

   function Find (L : Lists.List; K : Key_Type) return Element_Type is
      use Lists;
      I : Cursor := L.First;
   begin
      while Has_Element (I) loop
         if Key (Element (I)) = K then
            return Element (I);
         else
            Next (I);
         end if;
      end loop;

      raise Constraint_Error with "Key not found";
   end Find;

   --------------
   -- Contains --
   --------------

   function Contains (L : Lists.List;
                      K : Key_Type)
                      return Boolean
   is
      use Lists;
      I : Cursor := L.First;
   begin
      while Has_Element (I) loop
         if Key (Element (I)) = K then
            return True;
         end if;
         Next (I);
      end loop;
      return False;
   end Contains;

   -------------
   -- To_List --
   -------------

   function To_List (Src : Maps.Map) return Lists.List is
      L : Lists.List;
      use Maps;
      procedure Add (I : Cursor) is
      begin
         L.Append (Element (I));
      end Add;
   begin
      Src.Iterate (Add'Access);
      return L;
   end To_List;

end Agpl.Containers.Bulk.Utils;
