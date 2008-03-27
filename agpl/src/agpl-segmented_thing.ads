 

--  Support for segmented things (initially implemented for segmented files).
--  Allows to keep info about an object which is integrally composed of seg-
--  ments.
--  Semantics are:
--  Equal data adjacent segments are merged automatically.
--  No position is without segment.

with Ada.Containers.Ordered_Sets;

generic
   type Segment_Data  is private;
   type Index_Type    is range <>;
package Agpl.Segmented_Thing is

   pragma Preelaborate;

   ------------------------------------------------------------------------
   -- Object                                                             --
   ------------------------------------------------------------------------
   type Object is private;

   type Chunk_Type is record
      Data  : Segment_Data;
      First : Index_Type;
      Last  : Index_Type;
   end record;

   ------------------------------------------------------------------------
   -- Create                                                             --
   ------------------------------------------------------------------------
   --  Creates a segmented object with initially a single segment comprending
   --  all the object.
   --  It's illegal to call any other function without the object
   --  having been created.
   procedure Create (
      This         :    out Object;
      First        : in     Index_Type;
      Last         : in     Index_Type;
      Initial_Data : in     Segment_Data);

   ------------------------------------------------------------------------
   -- Count                                                              --
   ------------------------------------------------------------------------
   --  Says the number of segments.
   function Count (This : in Object) return Natural;

   ------------------------------------------------------------------------
   -- Indexes                                                            --
   ------------------------------------------------------------------------
   function First (This : in Object) return Index_Type;
   function Last  (This : in Object) return Index_Type;
   pragma Inline (First, Last);

   ------------------------------------------------------------------------
   -- Get                                                                --
   ------------------------------------------------------------------------
   --  Return a numbered piece
   --  Warning! cost O (N)
   function Get (This : in Object; Index : in Positive) return Chunk_Type;
   pragma Inline (Get);

   --  This has variable cost: O (1) for sequential advance.
   procedure Get (
      This  : in out Object;
      Index : in     Positive;
      Data  :    out Chunk_Type);

   ------------------------------------------------------------------------
   -- Get_At                                                             --
   ------------------------------------------------------------------------
   --  Return the data at given point
   function Get_At (This : in Object; Pos : in Index_Type) return Segment_Data;
   pragma Inline (Get_At);

   --  Or at a given ratio (1 .. Total):
   function Get_At (This : in Object; Offset, Total : in Index_Type) return Segment_Data;
   pragma Inline (Get_At);

   ------------------------------------------------------------------------
   -- Set                                                                --
   ------------------------------------------------------------------------
   --  Set data for a given segment. Splitting and merging are done if necessary.
   --  Last must point to the last element in the segment, not to the first in
   --  the next segment.
   procedure Set (
      This  : in out Object;
      First : in     Index_Type;
      Last  : in     Index_Type;
      Data  : in     Segment_Data);

   ------------------------------------------------------------------------
   -- Debug_Dump                                                         --
   ------------------------------------------------------------------------
   generic
      with function Image (Data : in Segment_Data) return String is <>;
   procedure Debug_Dump (This : in Object);

private

   function "<" (L, R : in Chunk_Type) return Boolean;
   function "=" (L, R : in Chunk_Type) return Boolean;
   pragma Inline ("<", "=");

   package Ordered_Segments is new Ada.Containers.Ordered_Sets (
      Chunk_Type, "<", "=");

   function Key (This : in Chunk_Type) return Index_Type; pragma Inline (Key);
   function "<" (L : in Index_Type; R : in Chunk_Type) return Boolean;
   pragma Inline ("<");
   function ">" (L : in Index_Type; R : in Chunk_Type) return Boolean;
   pragma Inline (">");

   package Ordered_Keys is new Ordered_Segments.Generic_Keys (
      Index_Type, Key, "<");

   --  Implementation detail: each segment starts at the ending position of the
   --  previous, plus one (discrete nature).

   type Object is record
      Segments : Ordered_Segments.Set;
      First    : Index_Type;
      Last     : Index_Type;

      --  We use these two to provide a moving cursor in the object and avoid O (n) cost.
      Pos      : Natural := 0; -- 0 means unpositioned, goes in 1 .. Length (Segments);
      Idx      : Ordered_Segments.Cursor;
   end record;

end Agpl.Segmented_Thing;
