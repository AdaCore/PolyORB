with Kernel.Peripherals;
with Kernel.Serial_Output;

with PolyORB.Utils.Strings;
with PolyORB.Initialization;

package body PolyORB.Log.ORK_Serial is

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (S : String);

   procedure Put_Line (S : String) is
   begin
      Kernel.Serial_Output.Put_Line (S);
   end Put_Line;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      --  Initialize ORK serial line
      Kernel.Serial_Output.Init_Serial_Line
        (Kernel.Peripherals.Serial_Port_1);

      PolyORB.Log.Internals.Log_Hook := Put_Line'Access;
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"log.ork",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => +"log",
       Implicit  => True,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Log.ORK_serial;
