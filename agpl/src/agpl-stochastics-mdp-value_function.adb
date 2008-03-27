with Agpl.Trace; use Agpl.Trace;

package body Agpl.Stochastics.Mdp.Value_Function is

   --------------
   -- Contains --
   --------------

   function Contains
     (This : in Object;
      S    : in State.Object'Class) return Boolean
   is
   begin
      return Contains (This, State.Get_Id (S));
   end Contains;

   --------------
   -- Contains --
   --------------

   function Contains
     (This : in Object;
      S    : in State.Object_Id) return Boolean
   is
      use Value_Maps;
   begin
      return Contains (This.Values, S);
   end Contains;

   ----------------
   -- Get_Action --
   ----------------

   function Get_Action
     (This : in Object;
      S    : in State.Object'Class) return Action.Object'Class
   is
   begin
      return Get_Action (This, State.Get_Id (S));
   end Get_Action;

   ----------------
   -- Get_Action --
   ----------------

   function Get_Action
     (This : in Object;
      S    : in State.Object_Id) return Action.Object'Class
   is
      use Action_Maps;
   begin
      if Contains (This.Actions, S) then
         return Element (Find (This.Actions, S));
      else
         raise Unknown_Value;
      end if;
   end Get_Action;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (This : in Object;
      S    : in State.Object'Class)
      return Rewards
   is
   begin
      return Get_Value (This, State.Get_Id (S));
   end Get_Value;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (This : in Object;
      S    : in State.Object_Id)
      return Rewards
   is
      use Value_Maps;
   begin
      if Contains (This.Values, S) then
         return Element (Find (This.Values, S));
      else
         raise Unknown_Value;
      end if;
   end Get_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (This  : in out Object;
      S     : in     State.Object'Class;
      Value : in     Rewards;
      A     : in     Action.Object'Class)
   is
      Success : Boolean;
      PV      : Value_Maps.Cursor;
      PA      : Action_Maps.Cursor;
      PS      : State.Object_Maps.Cursor;
      Sid     : constant State.Object_Id := State.Get_Id (S);
   begin
      if Value_Maps.Contains (This.Values, Sid) then
         Value_Maps.Replace (This.Values, Sid, Value);
         Action_Maps.Replace (This.Actions, Sid, A);
         State.Object_Maps.Replace (This.States, Sid, S);
      else
         Value_Maps.Insert (This.Values, Sid, Value, PV, Success);
         pragma Assert (Success);
         Action_Maps.Insert (This.Actions, Sid, A, PA, Success);
         pragma Assert (Success);
         State.Object_Maps.Insert (This.States, Sid, S, PS, Success);
         pragma Assert (Success);
      end if;
   end Set_Value;

   -------------
   -- Summary --
   -------------

   procedure Summary (This : in Object) is
      use Value_Maps;
      I : Value_Maps.Cursor := First (This.Values);
      type FP is delta 0.00001 digits 18;
      --  Float_Printer
   begin
      Log ("Value function summary: ", Debug, Section => Log_Section);
      while I /= No_Element loop
         Log
           ("State " & String (Key (I)) & " x " &
            Action.To_String
              (Action_Maps.Element
                 (Action_Maps.Find (This.Actions, Key (I)))) &
            " --> " & FP'Image (FP (Element (I))),
            Debug, Section => Log_Section);

         Next (I);
      end loop;
   end Summary;

   -----------
   -- First --
   -----------

   function First (This : in Object) return Cursor is
      use Value_Maps;
   begin
      if not Is_Empty (This.Values) then
         return
           (Valid => True,
            Pos   => First (This.Values));
      else
         return
           (Valid => False,
            Pos   => No_Element);
      end if;
   end First;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (This : in Cursor) return Boolean is
   begin
      return This.Valid;
   end Is_Valid;

   ----------
   -- Next --
   ----------

   procedure Next (This : in out Cursor) is
      use Value_Maps;
   begin
      Next (This.Pos);
      This.Valid := This.Pos /= No_Element;
   end Next;

   ----------------
   -- Get_Action --
   ----------------

   function Get_Action
     (This : in Object; I : in Cursor) return Action.Object'Class
   is
   begin
      return Action_Maps.Element
        (Action_Maps.Find
           (This.Actions, Value_Maps.Key (I.Pos)));
   end Get_Action;

   ----------------
   -- Get_Reward --
   ----------------

   function Get_Reward
     (This : in Object; I : in Cursor) return Rewards
   is
      pragma Unreferenced (This);
      use Value_Maps;
   begin
      return Element (I.Pos);
   end Get_Reward;

      ---------------
      -- Get_State --
      ---------------

   function Get_State
     (This : in Object; I : in Cursor) return State.Object'Class
   is
      use State.Object_Maps;
   begin
      return Element (Find (This.States, Value_Maps.Key (I.Pos)));
   end Get_State;

   ------------------
   -- Get_State_Id --
   ------------------

   function Get_State_Id
     (This : in Object; I : in Cursor) return State.Object_Id
   is
      pragma Unreferenced (This);
      use Value_Maps;
   begin
      return Key (I.Pos);
   end Get_State_Id;

end Agpl.Stochastics.Mdp.Value_Function;
