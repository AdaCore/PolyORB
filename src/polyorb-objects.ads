------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      P O L Y O R B . O B J E C T S                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Root type for concrete object implementations (servants).

--  $Id$

with Ada.Streams;

with PolyORB.Components;

package PolyORB.Objects is

   pragma Elaborate_Body;

   type Object_Id is new Ada.Streams.Stream_Element_Array;
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
