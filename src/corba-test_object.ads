--  A simple test server object.

--  $Id$

with Droopi.Components;
with CORBA.POA_Types;
with CORBA;

package CORBA.Test_Object is
   use CORBA;

   type My_Object is new CORBA.POA_Types.Servant with null record;

   procedure Create (O : in out My_Object);

   function "=" (Left, Right : My_Object)
     return Standard.Boolean;

   function echoString
     (O : My_Object;
      S : CORBA.String)
     return CORBA.String;

   function echoInteger
     (O : My_Object;
      I : CORBA.Long)
     return CORBA.Long;

   function Handle_Message
     (Obj : access My_Object;
      Msg : Droopi.Components.Message'Class)
     return Droopi.Components.Message'Class;

end CORBA.Test_Object;

