--  GENERIC PACKAGE FOR HANDLING PERMUTATIONS OF DISCRETE ITEMS
   -----------------------------------------------------------

--  Revision : 15-OCT-1990 by Mats Weber, changed generic parameter of Random_Permutation
--                                       in order to avoid trial-and-error in its
--                                       implementation.
--  Revision : 31-JAN-1989 by Mats Weber, added generic function RANDOM_PERMUTATION.

--  Creation : 13-JAN-1989 by Mats Weber.


generic
   type Discrete is (<>);    -- Must have at least two values
   type Count is range <>;
package Permutations is

   pragma Preelaborate;

   subtype Positive_Count is Count range 1..Count'Last;

   type Permutation is array (Discrete) of Discrete;
   ----------------

   subtype Cycle_Length is Count range 2..Permutation'Length;

   type Discrete_List is array (Positive_Count range <>) of Discrete;

   type Cycle (Length : Cycle_Length := 2) is
   ----------
      record
         Contents : Discrete_List(1..Length);
      end record;

   type Cycle_List is array (Positive_Count range <>) of Cycle;

   subtype Transposition is Cycle(Length => 2);
   ---------------------

   type Transposition_List is array (Positive_Count range <>) of Transposition;

   type Parity is (Even, Odd);
   -----------


   function Identity return Permutation;

   function To_Permutation (The_Cycle : Cycle) return Permutation;

   function Equal (Left, Right : Cycle) return Boolean;
      --  Returns TRUE if and only if LEFT and RIGHT represent the same permutation.

   function "*" (Left : Permutation; Right : Permutation) return Permutation;
   function "*" (Left : Permutation; Right : Cycle)       return Permutation;
   function "*" (Left : Cycle;       Right : Permutation) return Permutation;
   function "*" (Left : Cycle;       Right : Cycle)       return Permutation;
      --  Returns the permutation A -> LEFT(RIGHT(A)) for all A of type DISCRETE.

   function Inverse (Of_Permutation : Permutation) return Permutation;
   function Inverse (Of_Cycle       : Cycle)       return Cycle;

   function Decomposition (Of_Permutation : Permutation) return Cycle_List;
   function Decomposition (Of_Permutation : Permutation) return Transposition_List;
   function Decomposition (Of_Cycle       : Cycle)       return Transposition_List;

   function Signature (Of_Permutation : Permutation) return Parity;
   function Signature (Of_Cycle       : Cycle)       return Parity;

   function "*" (Left, Right : Parity) return Parity;

   generic
      with procedure Action (A_Permutation : in Permutation);
   procedure Enumeration;
      --  Enumerates all possible permutations of discrete.

   generic
      with function Uniform (First, Last : Count) return Count;
         --  must return a uniformly distributed Count in the
         --  range First..Last.
   function Random_Permutation return Permutation;
      --  Returns a uniformly distributed random permutation.

end Permutations;
