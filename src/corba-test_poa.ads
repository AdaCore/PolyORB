with CORBA.POA_Types;
with Droopi.Components;

package CORBA.Test_POA is

   Incorrect_Execution : exception;
   Correct_Execution   : exception;

   type My_Servant is new CORBA.POA_Types.Servant with
     record
        Nb   : Integer;
        Name : String;
     end record;
   type My_Servant_Access is access all My_Servant;

   function Handle_Message
     (S   : access My_Servant;
      Msg : Droopi.Components.Message'Class)

     return Droopi.Components.Message'Class;

   function "=" (Left, Right : My_Servant)
     return Standard.Boolean;

   procedure Test_The_POA;

end CORBA.Test_POA;
