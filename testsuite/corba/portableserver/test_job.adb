with PolyORB.Utils.Report;
with CORBA;

package body Test_Job is

   procedure Run_Job is
   begin
      PolyORB.Utils.Report.Output
        ("Invocation on servant finished",
         "Hello Ada World !" =
         CORBA.To_Standard_String
         (Echo.echoString (Global_Obj_Ref,
                           CORBA.To_CORBA_String ("Hello Ada World !"))));
   end Run_Job;

   procedure Run_Job_Wait is
   begin
      PolyORB.Utils.Report.Output
        ("Invocation on servant finished",
         "Hello Ada World !" =
         CORBA.To_Standard_String
         (Echo.EchoString_Wait
          (Global_Obj_Ref,
           CORBA.To_CORBA_String ("Hello Ada World !"))));
   end Run_Job_Wait;

end Test_Job;
