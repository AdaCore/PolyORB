--  Root type for concrete object implementations (servants).

--  $Id$

with Ada.Streams; use Ada.Streams;

with Droopi.Components;

package Droopi.Objects is

   type Object_Id is new Stream_Element_Array;
   --  XXX ???

   type Servant is abstract new Droopi.Components.Component
     with private;
   --  XXX or: is new Droopi.Refs.Entity?
   type Servant_Access is access all Servant'Class;
   --  A Servant is a Component that supports the messages
   --  defined in Droopi.Objects.Interface.

   function Handle_Message
     (S : Servant;
      Msg : Components.Message'Class)
     return Components.Message'Class is abstract;

private

   type Servant is abstract new Droopi.Components.Component
     with null record;

end Droopi.Objects;
