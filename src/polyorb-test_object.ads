--  A simple test server object.

--  $Id$

with PolyORB.Components;
with PolyORB.Obj_Adapters.Simple;
with PolyORB.Objects;
with PolyORB.Types;

package PolyORB.Test_Object is

   pragma Elaborate_Body;

   use PolyORB.Types;

   type My_Object is new PolyORB.Objects.Servant with null record;

   function waitAndEchoString
     (O : My_Object;
      S : Types.String;
      T : Types.Long)
     return Types.String;

   function echoString
     (O : My_Object;
      S : Types.String)
     return Types.String;

   function echoInteger
     (O : My_Object;
      I : Types.Long)
     return Types.Long;

   function Handle_Message
     (Obj : access My_Object;
      Msg : Components.Message'Class)
     return Components.Message'Class;

   function If_Desc
     return Obj_Adapters.Simple.Interface_Description;
   pragma Inline (If_Desc);

end PolyORB.Test_Object;

