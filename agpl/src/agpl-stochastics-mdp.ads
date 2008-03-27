 

package Agpl.Stochastics.Mdp is

   pragma Preelaborate;

   Log_Section : constant String := "agpl.stochastics.mdp";

   --  Really, Markov matrix are square and its rows sum 1. This isn't
   --  enforceable in the type declaration so...
   type Markov_Matrix is
     array (Positive range <>, Positive range <>) of Probabilities;

   type Discounts is new Float range 0.0 .. 1.0;

   type Rewards is new Float;
   subtype Costs is Rewards;
   --  Costs and Rewards are equivalent, just the sign changes.

   type Reward_Array is array (Positive range <>) of Rewards;

   type Distances is new Float;

   Verbose : Boolean := False;
   --  If true, detailed progress messages will be printed by the solvers.

end Agpl.Stochastics.Mdp;
