package body Agpl.Stochastics.Mdp.Problem is

   ------------
   -- Create --
   ------------

   procedure Create
     (This            :    out Object;
      Initial_States  : in     State.Object_Lists.List;
      Final_States    : in     State.Object_Lists.List;
      Actions         : in     Action.Object_Lists.List;
      Discount        : in     Discounts := 0.95)
   is
   begin
      This.Initial_States := Initial_States;
      This.Final_States   := Final_States;
      This.Actions        := Actions;
      This.Discount       := Discount;
   end Create;

   -----------------
   -- Get_Actions --
   -----------------

   function Get_Actions
     (This : in Object)
      return Action.Object_Lists.List
   is
   begin
      return This.Actions;
   end Get_Actions;

   ------------------
   -- Get_Discount --
   ------------------

   function Get_Discount (This : in Object) return Discounts is
   begin
      return This.Discount;
   end Get_Discount;

      ----------------------
      -- Get_Final_States --
      ----------------------

   function Get_Final_States (This : in Object)
                              return State.Object_Lists.List
   is
   begin
      return This.Final_States;
   end Get_Final_States;

   ------------------------
   -- Get_Initial_States --
   ------------------------

   function Get_Initial_States
     (This : in Object)
      return State.Object_Lists.List
   is
   begin
      return This.Initial_States;
   end Get_Initial_States;

end Agpl.Stochastics.Mdp.Problem;
