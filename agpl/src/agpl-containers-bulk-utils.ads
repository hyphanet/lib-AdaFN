

--  Conversions for containers

generic
package Agpl.Containers.Bulk.Utils is

   pragma Preelaborate;

   --  CONVERSIONS BETWEEN CONTAINERS

   function To_Vector (Src : in Lists.List) return Vectors.Vector;
   function "+"       (Src : in Lists.List) return Vectors.Vector renames To_Vector;

   function To_Map (Src : Lists.List;
                    Key : access function (E : Element_Type) return Key_Type)
                    return Maps.Map;

   -- LIST UTILS --

   procedure Delete (L : in out Lists.List;
                     K :        Key_Type;
                     Fail_If_Missing : Boolean := True);
   --  Remove the *first* occurrence of K

   function Find (L : Lists.List; K : Key_Type) return Element_Type;
   function Contains (L : Lists.List;
                      K : Key_Type)
                      return Boolean;

   function To_List (Src :    Element_Type) return Lists.List;
   function "+" (Src :    Element_Type) return Lists.List renames To_List;
   --  construct a list containing a single item

   function To_List (Src : in Vectors.Vector) return Lists.List;
   function "+"     (Src : in Vectors.Vector) return Lists.List renames To_List;

   function To_List (Src : Maps.Map) return Lists.List;
   function "+"     (Src : Maps.Map) return Lists.List renames To_List;

   function To_Key_Set (Src                 : Lists.List;
                        Allow_Repeated_Keys : Boolean := False)
                        return Key_Sets.Set;

   procedure Concatenate (Dest : in out Lists.List; Src : in Lists.List);
   procedure Append (L : in out Lists.List; R : Lists.List) renames Concatenate;
   function Slice (L     : Lists.List;
                   First : Positive;
                   Last  : Natural) return Lists.List;
   --  Gets a slice. No checks for out of range (apart from the container ones)

   function Intersect (L, R : Lists.List) return Lists.List;
   --  Return L with elements also in R, respecting L order.
   function "and" (L, R : Lists.List) return Lists.List renames Intersect;
   function "*"   (L, R : Lists.List) return Lists.List renames Intersect;

   function "+" (L, R : Lists.List) return Lists.List;

   function "-" (L, R : Lists.List) return Lists.List;
   --  Return L without any elements existing in R
   --  O (N^2)

   function "-" (L : Lists.List; R : Element_Type) return Lists.List;
   --  O (N)

   -- MAP UTILS --

   function To_Map (Key : Key_Type; Src : Element_Type) return Maps.Map;
   function "+"    (Key : Key_Type; Src : Element_Type) return Maps.Map
                    renames To_Map;

   -- VECTOR UTILS --

   function Reverse_Slice (Src : in Vectors.Vector;
                           Ini : in Index_Type;
                           Fin : in Index_Type) return Vectors.Vector;
   --  returns a copy of Src but with the ini..fin part reversed.
   --  Has the same length as src thus

end Agpl.Containers.Bulk.Utils;
