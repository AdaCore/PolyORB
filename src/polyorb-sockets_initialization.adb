with GNAT.Sockets;

with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization); --  WAG:3.15

with PolyORB.Utils.Strings;

package body PolyORB.Sockets_Initialization is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      GNAT.Sockets.Initialize;
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"sockets",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => Empty,
       Init      => Initialize'Access));

end PolyORB.Sockets_Initialization;
