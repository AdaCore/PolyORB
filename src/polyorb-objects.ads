--  Root type for concrete object implementations (servants).

--  $Id$

with Ada.Streams; use Ada.Streams;

with PolyORB.Components;

package PolyORB.Objects is

   pragma Elaborate_Body;

   type Object_Id is new Stream_Element_Array;
   type Object_Id_Access is access all Object_Id;

   procedure Free (X : in out Object_Id_Access);

   function To_String (Oid : Object_Id) return String;
   function To_Oid (S : String) return Object_Id;
   --  Generic helper functions: convert an oid from/to
   --  a printable string representation.

   function Image (Oid : Object_Id) return String;
   --  For debugging purposes.

   type Servant is abstract new PolyORB.Components.Component
     with private;
   type Servant_Access is access all Servant'Class;
   --  A Servant is a Component that supports the messages
   --  defined in PolyORB.Objects.Interface. This type may
   --  be further derived by personality-specific units.

   function Handle_Message
     (S   : access Servant;
      Msg : Components.Message'Class)
     return Components.Message'Class is abstract;

private

   pragma Inline (Free);
   pragma Inline (To_String);
   pragma Inline (To_Oid);

   type Servant is abstract new PolyORB.Components.Component
     with null record;

end PolyORB.Objects;
