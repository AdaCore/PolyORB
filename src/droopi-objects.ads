--  Root type for concrete object implementations (servants).

--  $Id$

with Ada.Streams; use Ada.Streams;

package Droopi.Objects is

   pragma Preelaborate;

   type Object_Id is new Stream_Element_Array;
   --  XXX ???

   type Servant is abstract tagged limited private;
   --  XXX or: is new Droopi.Refs.Entity?
   type Servant_Access is access all Servant'Class;

private

   type Servant is abstract tagged limited null record;

end Droopi.Objects;
