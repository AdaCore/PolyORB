--  A simple test server object.

--  $Id$

with Droopi.Components;
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

--  private

   function Handle_Message
     (O   : access My_Object;
      Msg : Components.Message'Class)
     return Components.Message'Class;

end Droopi.Test_Object;

