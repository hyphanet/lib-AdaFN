 

--  Provides a one-way hash table with a single value per key.

with Agpl.Types.Ustrings; use Agpl.Types.Ustrings;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

generic
   type Element_Type is private;
   with function Equal (L, R : in Element_Type) return Boolean;
package Agpl.Simple_Dictionary is

   pragma Preelaborate;

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   type Object is private;

   type Pairs is record
      Key   : Ustring;
      Value : Element_Type;
   end record;
   type Pair_Array is array (Positive range <>) of Pairs;

   ------------------------------------------------------------------------
   -- Add_Word                                                           --
   ------------------------------------------------------------------------
   --  Add a word with given index (key).
   procedure Add_Word (This : in out Object; Key : in String; Element : in Element_Type);

   ------------------------------------------------------------------------
   -- Are_Compatible                                                     --
   ------------------------------------------------------------------------
   --  True if elements in both containers are equal, extra are ignored.
   --  At least one element must match.
   --  Commutative.
   function Are_Compatible (L, R : in Object) return Boolean;

   ------------------------------------------------------------------------
   -- Contains_Key                                                       --
   ------------------------------------------------------------------------
   --  True if the dictionary contains the given key
   function Contains_Key (This : in Object; Key : in String) return Boolean;

   ------------------------------------------------------------------------
   -- Get_Contents                                                       --
   ------------------------------------------------------------------------
   --  Return an array of contents in the dictionary
   function Get_Contents (This : in Object) return Pair_Array;

   ------------------------------------------------------------------------
   -- Get_Value                                                          --
   ------------------------------------------------------------------------
   function Get_Value (This : in Object; Key : in String) return Element_Type;

   ------------------------------------------------------------------------
   -- Merge                                                              --
   ------------------------------------------------------------------------
   --  Adds elements not in Former from Later.
   --  No compatibility check is performed
   procedure Merge (Former : in out Object; Later : in Object);

private

   package Element_Map is new Ada.Containers.Indefinite_Hashed_Maps (
      String, Element_Type, Ada.Strings.Hash, "=", Equal);

   type Object is new Element_Map.Map with null record;

   pragma Inline (Add_Word);

end Agpl.Simple_Dictionary;
