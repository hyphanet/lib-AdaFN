--  A collection of protected data with notifiers on modification

package body Agpl.Protected_Datastore is

   ----------------------
   -- Notify_Listeners --
   ----------------------

   procedure Notify_Listeners (This : in out Object;
                               Key   : in     Object_Key;
                               Value : in     Object_Data'Class)
   is
      use Listener_Vectors;
      V : constant Vector := This.Safe.Get_Callbacks (Key);
   begin
      for I in First_Index (V) .. Last_Index (V) loop
         On_Key_Stored (Element (V, I).all,
                        Key,
                        Value);
      end loop;
   end Notify_Listeners;

   --------------
   -- Contains --
   --------------

   function Contains (This : in Object;
                      Key  : in Object_Key)
                      return    Boolean
   is
   begin
      return This.Safe.Contains (Key);
   end Contains;

   ---------
   -- Get --
   ---------

   function Get (This : in Object;
                 Key  : in Object_Key)
                 return    Object_Data'Class
   is
   begin
      return This.Safe.Get (Key);
   end Get;

   ---------
   -- Put --
   ---------

   procedure Put (This  : in out Object;
                  Key   : in     Object_Key;
                  Value : in     Object_Data'Class) is
   begin
      This.Safe.Put (Key, Value);

      Notify_Listeners (This, Key, Value);
   end Put;

   ------------
   -- Listen --
   ------------

   procedure Listen (This     : in out Object;
                     Key      : in     Object_Key;
                     Listener : not null Key_Listener_Access) is
   begin
      This.Safe.Listen (Key, Listener);

      --  Already call if key existed
      if This.Contains (Key) then
         Listener.On_Key_Stored (Key, This.Get (Key));
      end if;
   end Listen;

   ------------
   -- Update --
   ------------

   procedure Update (This : in out Object;
                     Key  : in     Object_Key;
                     Fun  : in out Functor'Class)
   is
      Val : Object_Data_Handle.Object;
   begin
      This.Safe.Update (Key, Fun, Val);
      Notify_Listeners (This, Key, Val.Get);
   end Update;

   -----------------
   -- Safe_Object --
   -----------------

   protected body Safe_Object is

      --------------
      -- Contains --
      --------------

      function Contains (Key : in Object_Key)
                         return   Boolean
      is
         use Key_Object_Maps;
      begin
         return Has_Element (Find (Values, Key));
      end Contains;

      ---------
      -- Put --
      ---------

      procedure Put (Key   : in Object_Key;
                     Value : in Object_Data'Class)
      is
         use Key_Listener_Maps;
      begin
         Key_Object_Maps.Include (Values, Key, Value);
      end Put;

      ---------
      -- Get --
      ---------

      function Get (Key : in Object_Key)
                    return   Object_Data'Class
      is
         use Key_Object_Maps;
         I : constant Cursor := Find (Values, Key);
      begin
         if Has_Element (I) then
            return Element (I);
         else
            raise Data_Not_Present;
         end if;
      end Get;

      -------------------
      -- Get_Callbacks --
      -------------------

      function Get_Callbacks (Key : in Object_Key)
                              return   Listener_Vectors.Vector
      is
         use Key_Listener_Maps;
         I : constant Cursor := Find (Callbacks, Key);
      begin
         if Has_Element (I) then
            return Element (I);
         else
            return Listener_Vectors.Empty_Vector;
         end if;
      end Get_Callbacks;

      ------------
      -- Listen --
      ------------

      procedure Listen (Key      : in Object_Key;
                        Listener : in Key_Listener_Access)
      is
         use Key_Listener_Maps;
         I  : Cursor := Find (Callbacks, Key);
         Ok : Boolean;
      begin
         if not Has_Element (I) then
            Insert (Callbacks,
                    Key,
                    New_Item => Listener_Vectors.Empty_Vector,
                    Position => I,
                    Inserted => Ok);
            pragma Assert (Ok);
         end if;

         declare
            procedure Add (K : in Object_Key; V : in out Listener_Vectors.Vector)
            is
               pragma Unreferenced (K);
            begin
               V.Append (Listener);
            end Add;
         begin
            Update_Element (Callbacks, I, Add'Access);
         end;
      end Listen;

      ------------
      -- Update --
      ------------

      procedure Update (Key  : in     Object_Key;
                        Fun  : in out Functor'Class;
                        Res  :    out Object_Data_Handle.Object)
      is
         use Key_Object_Maps;
         I : constant Cursor := Values.Find (Key);
      begin
         if Has_Element (I) then
            declare
               procedure Update (Key : Object_Key; Data : in out Object_Data'Class) is
                  pragma Unreferenced (Key);
               begin
                  Fun.Operate (Data);
                  Res.Set (Data);
               end Update;
            begin
               Values.Update_Element (I, Update'Access);
            end;
         else
            raise Data_Not_Present;
         end if;
      end Update;

   end Safe_Object;

end Agpl.protected_Datastore;
