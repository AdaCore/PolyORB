with CosEventComm; use CosEventComm;

with PolyORB.Setup.Thread_Pool_Server;
pragma Elaborate_All (PolyORB.Setup.Thread_Pool_Server);
pragma Warnings (Off, PolyORB.Setup.Thread_Pool_Server);

with CORBA;

with CORBA.Impl;


with PolyORB.Tasking.Mutexes;
with PolyORB.Tasking.Condition_Variables;

use PolyORB.Tasking.Condition_Variables;
use PolyORB.Tasking.Mutexes;


package Auto_Print is

   procedure Auto_Display;

   procedure Ensure_Initialization;
   pragma Inline (Ensure_Initialization);
   --  Ensure that the Mutexes are initialized

   T_Initialized : Boolean := False;

   Session_Mutex : Mutex_Access;
   Session_Taken : Condition_Access;
   --  Synchornisation of task initialization.

   EndDisplay : Boolean := False;

   A_S : CORBA.Impl.Object_Ptr := null;
   --  This variable is used to initialize the threads local variable.
   --  it is used to replace the 'accept' statement.

end Auto_Print;
