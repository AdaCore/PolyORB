with PolyORB.Components;

use PolyORB;

package AWS.Server.Servants is

   type Web_Servant is new HTTP with null record;
   type SOAP_Servant is new HTTP with null record;
   --  2 servants are associated to an AWS Web server

private

   type Web_Servant_Acc is access all Web_Servant;
   type SOAP_Servant_Acc is access all SOAP_Servant;

   function Execute_Servant
     (S   : access Web_Servant;
      Msg : Components.Message'Class)
     return Components.Message'Class;

   function Execute_Servant
     (S   : access SOAP_Servant;
      Msg : Components.Message'Class)
     return Components.Message'Class;

end AWS.Server.Servants;
