package body Agpl.Stochastics.Mdp.Outcome is

   ------------
   -- Create --
   ------------

   function Create
     (A : in Action.Object'Class;
      S : in State.Object_Lists.List)
      return Object
   is
      This : Object;
   begin
      This.Action.Append (A);
      This.States := S;

      return This;
   end Create;

   ----------------
   -- Get_Action --
   ----------------

   function Get_Action (This : in Object) return Action.Object'Class is
      use Action.Object_Lists;
   begin
      return Element (First (This.Action));
   end Get_Action;

   function Get_Action (This : in Object) return Action.Object_Lists.List is
   begin
      return This.Action;
   end Get_Action;

   ----------------
   -- Get_States --
   ----------------

   function Get_States (This : in Object) return State.Object_Lists.List is
   begin
      return This.States;
   end Get_States;

end Agpl.Stochastics.Mdp.Outcome;
