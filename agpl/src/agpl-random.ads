 

with Ada.Numerics.Float_Random;

--  Shortcuts to some common random utilities.
--  This package if *not* thread safe.

--  During elaboration, the internal random generator is reset. This makes
--  each program run not repeatable.

package Agpl.Random is

   --  pragma Elaborate_Body;

   subtype Uniformly_Distributed is
     Ada.Numerics.Float_Random.Uniformly_Distributed;

   subtype Open_Uniformly_Distributed is Float range 0.0 .. Float'Pred (1.0);

   function Open_Uniform return Open_Uniformly_Distributed;
   --  Quick obtention of a random point in [0.0 .. 1.0)

   function Uniform return Uniformly_Distributed;
   --  Quick obtention of a random point in [0.0 .. 1.0]

   function Get_Float (Min, Max : in Float) return Float;
   --  Get a float in [Min .. Max]

   generic
      type Discrete is (<>);
   function Uniform_Discrete return Discrete;
   --  Obtain a random value from a discrete type.

   function Get_Integer (Min, Max : in Integer) return Integer;
   --  Quick obtention of a random integer in [Min, Max]
   --  if Max < Min then Max is returned

   type Object is tagged limited private;
   --  Use this object to have repeteability.
   --  Unless reset, this object has a default initialization which will make
   --  all runs identic.

   function Open_Uniform (This : in Object) return Open_Uniformly_Distributed;

   procedure Reset (Initiator : in Integer);
   --  Reset the internal generator.

   procedure Reset (This : in out Object);
   --  This resets based in some clock value.

   procedure Reset (This : in out Object; Initiator : in Integer);
   --  This resets to a certain generator.

   function Uniform (This : in Object) return Uniformly_Distributed;

   function Get_Integer (This     : in Object;
                         Min, Max : in Integer) return Integer;
   --  if Max < Min then Max is returned

   function Get_Float (This     : in Object;
                       Min, Max : in Float) return Float;

   generic
      type Discrete is (<>);
   function Discrete_Random (This : in Object) return Discrete;

   function Flip_Coin return Boolean;
   --  50% chance of true

private

   type Object is tagged limited record
      Gen : Ada.Numerics.Float_Random.Generator;
   end record;

end Agpl.Random;
