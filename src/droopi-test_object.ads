--  A simple test server object.

--  $Id$

with Droopi.Components;
with Droopi.Obj_Adapters.Simple;
with Droopi.Objects;

package Droopi.Test_Object is

   type My_Object is new Droopi.Objects.Servant with null record;

   function Echo_String
     (O : My_Object;
      S : String)
     return String;

   function Echo_Integer
     (O : My_Object;
      I : Integer)
     return Integer;

   function Handle_Message
     (Obj : access My_Object;
      Msg : Components.Message'Class)
     return Components.Message'Class;

   function If_Desc
     return Obj_Adapters.Simple.Interface_Description;
   pragma Inline (If_Desc);

end Droopi.Test_Object;

