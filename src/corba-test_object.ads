--  A simple test server object.

--  $Id$

with PolyORB.Components;
with PolyORB.POA_Types;
with CORBA;

package CORBA.Test_Object is

   pragma Elaborate_Body;

   use CORBA;

   type My_Object is new PolyORB.POA_Types.Servant with null record;

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
      Msg : PolyORB.Components.Message'Class)
     return PolyORB.Components.Message'Class;

end CORBA.Test_Object;

