--
--  $Id$
--

with Interfaces.C; use Interfaces.C;

package body Timing is

   subtype Clock_T is long;

   type Tms is record
      Tms_Utime  : Clock_T;
      Tms_Stime  : Clock_T;
      Tms_Cutime : Clock_T;
      Tms_Cstime : Clock_T;
   end record;
   pragma Convention (C, Tms);

   function Sysconf (What : int := 3) return long;
   pragma Import (C, Sysconf, "_sysconf");

   Factor   : constant long := Sysconf;

   procedure Times (Buffer : access Tms);
   pragma Import (C, Times);

   Tms_Data : aliased Tms;

   -------------
   -- Current --
   -------------

   function Current return Milliseconds is
   begin
      Times (Tms_Data'Access);
      return Milliseconds ((1_000 *
                            (Tms_Data.Tms_Stime + Tms_Data.Tms_Utime))
                           / Factor);
   end Current;

end Timing;
