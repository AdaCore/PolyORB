with PolyORB.Report;
with CORBA;

package body Test_Job is

   procedure Run_Job is
   begin
      PolyORB.Report.Output
        ("Invocation on servant finished",
         "Hello Ada World !" =
         CORBA.To_Standard_String
         (Echo.echoString (Global_Obj_Ref,
                           CORBA.To_CORBA_String ("Hello Ada World !"))));
   end Run_Job;
end Test_Job;
