with Agpl.Text_Io; use Agpl.Text_Io;
with Agpl.Trace;

procedure T021_Trace_Zero_Overhead is
   use Agpl;
begin
   Put_Line ("Call 1");

   Trace.Log ("Ahoy!", Trace.Always);
   --  This line should not appear in the assembly.

   Put_Line ("Call 2");
end T021_Trace_Zero_Overhead;
