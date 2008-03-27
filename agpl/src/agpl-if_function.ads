 

--  The C (cond ? <exp> : <exp>) triple operator.

generic
   type Value (<>) is private;
function Agpl.If_Function (Condition : in Boolean;
                           If_True   : in Value;
                           If_False  : in Value) return Value;

pragma Inline (Agpl.If_Function);
pragma Preelaborate (Agpl.If_Function);
