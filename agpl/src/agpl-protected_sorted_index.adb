 

--  Implements a double indexed container, with sorted semantics and
--  search by id.
--  However, currently the search isn't efficient (takes O (n)).
--  It's ok for use with not huge lists.

package body Agpl.Protected_sorted_index is

   use Implementation;

   protected body Sorted_index is

      procedure Clear is
      begin
         Clear (List);
      end Clear;

      --  No duplicates (replacement)
      procedure Insert (Item : in Element_type) is
         Success : Boolean;
      begin
         Delete (Item, Success);       -- Local
         Insert (List, Item); -- Implementation
      end Insert;

      --  Local, optimized search
      function Find (Item : in Element_type) return Cursor is
         Fr : Cursor          := Floor (List, Item);
         Bk : constant Cursor := Ceiling (List, Item);
      begin
         while Fr /= No_Element loop
            if Item = Element (Fr) then
               return Fr;
            else
               Fr := Next (Fr);
            end if;

            exit when Fr = Bk;
         end loop;

         return No_Element;
      end Find;

      function Find (Item : in Element_type) return Boolean is
      begin
         return Find (Item) /= No_Element;
      end Find;

      procedure Delete (Item : in Element_type; Success : out Boolean) is
         I : Cursor := Find (Item); -- Ours!
      begin
         Success := I /= No_Element;
         if Success then
            Delete (List, I);
         end if;
      end Delete;

      --  Blocking if empty
      --  Doesn't remove it
      entry Get_first (Item : out Element_type) when not Is_empty is
      begin
         Item := Element (First (List));
      end Get_first;

      --  Get first if possible:
      procedure Get_first (Item : out Element_type; Success : out Boolean) is
      begin
         Success := not Is_empty;
         if Success then
            Item := Element (First (List));
         end if;
      end Get_first;

      --  Get and remove an element if found, nothing else.
      procedure Get_remove (
         Item    : in out Element_type; Found : out Boolean) is
         I       : Cursor;
      begin
         I := Find (Item); -- Local
         Found := I /= No_Element;
         if Found then
            Item := Element (I);
            Delete (List, I);
         end if;
      end Get_remove;

      --  Get first if exists and remove it
      procedure Get_first_remove (
         Item : out Element_type; Found : out Boolean)
      is
         I    : Cursor;
      begin
         Found := not Is_empty;
         if Found then
            I := First (List);
            Item := Element (I);
            Delete (List, I);
         end if;
      end Get_first_remove;

      function Is_empty return Boolean is
      begin
         return Is_empty (List);
      end Is_empty;

      function Length return Natural is
      begin
         return Natural (Length (List));
      end Length;

   end Sorted_index;

end Agpl.Protected_sorted_index;
