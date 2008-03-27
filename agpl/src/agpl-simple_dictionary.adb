 

--  Provides a one-way hash table with a single value per key.

package body Agpl.Simple_Dictionary is

   use Element_Map;

   ------------------------------------------------------------------------
   -- Add_Word                                                           --
   ------------------------------------------------------------------------
   --  Add a word with given index (key).
   procedure Add_Word (This : in out Object; Key : in String; Element : in Element_Type) is
   begin
      Include (This, Key, Element);

      --  Correctly replaced?
      pragma Assert (Element_Map.Element (Find (This, Key)) = Element);
   end Add_Word;

   ------------------------------------------------------------------------
   -- Are_Compatible                                                     --
   ------------------------------------------------------------------------
   --  True if elements in both containers are equal, extra are ignored.
   --  Commutative.
   function Are_Compatible (L, R : in Object) return Boolean is
      I, J    : Cursor;
      Matched : Boolean := False;
   begin
      I := First (L);
      while Has_Element (I) loop
         J := Find (R, Key (I));
         if Has_Element (J) then
            if not Equal (Element (I), Element (J)) then
               return False;
            else
               Matched := True;
            end if;
         end if;
         Next (I);
      end loop;

      return Matched;
   end Are_Compatible;

   ------------------------------------------------------------------------
   -- Contains_Key                                                       --
   ------------------------------------------------------------------------
   --  True if the dictionary contains the given key
   function Contains_Key (This : in Object; Key : in String) return Boolean is
   begin
      return Contains (This, Key);
   end Contains_Key;

   ------------------------------------------------------------------------
   -- Get_Contents                                                       --
   ------------------------------------------------------------------------
   --  Return an array of contents in the dictionary
   function Get_Contents (This : in Object) return Pair_Array is
      I   : Cursor := First (This);
      Res : Pair_Array (1 .. Integer (Length (This)));
   begin
      for J in Res'Range loop
         Res (J).Key   := U (Key (I));
         Res (J).Value := Element (I);
         Next (I);
      end loop;
      return Res;
   end Get_Contents;

   ------------------------------------------------------------------------
   -- Get_Value                                                          --
   ------------------------------------------------------------------------
   function Get_Value (This : in Object; Key : in String) return Element_Type is
   begin
      return Element (Find (This, Key));
   end Get_Value;

   ------------------------------------------------------------------------
   -- Merge                                                              --
   ------------------------------------------------------------------------
   --  Adds elements not in Former from Later.
   --  No compatibility check is performed
   procedure Merge (Former : in out Object; Later : in Object) is
      I : Cursor := First (Later);
   begin
      while Has_Element (I) loop
         Add_Word (Former, Key (I), Element (I));
         Next (I);
      end loop;
   end Merge;

end Agpl.Simple_Dictionary;
