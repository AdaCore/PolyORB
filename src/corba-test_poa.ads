with PolyORB.POA_Types;
with PolyORB.Components;

package CORBA.Test_POA is

   pragma Elaborate_Body;

   Incorrect_Execution : exception;
   Correct_Execution   : exception;

   type My_Servant is new PolyORB.POA_Types.Servant with
     record
        Nb   : Integer;
        Name : String;
     end record;
   type My_Servant_Access is access all My_Servant;

   function Handle_Message
     (S   : access My_Servant;
      Msg : PolyORB.Components.Message'Class)

     return PolyORB.Components.Message'Class;

   function "=" (Left, Right : My_Servant)
     return Standard.Boolean;

   procedure Test_The_POA;

end CORBA.Test_POA;
