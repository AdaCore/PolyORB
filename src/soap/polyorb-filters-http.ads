------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 P O L Y O R B . F I L T E R S . H T T P                  --
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

--  $Id$

--  HTTP protocol implementation (as a filter so higher-level protocol
--  engines can be plugged on it.)

with Ada.Streams;
with PolyORB.Buffers;
with PolyORB.HTTP_Methods;
with PolyORB.ORB;
with PolyORB.Types;

package PolyORB.Filters.HTTP is

   pragma Elaborate_Body;

   type HTTP_Filter_Factory is new Factory with private;

   procedure Create
     (Fact   : access HTTP_Filter_Factory;
      Filt   : out Filter_Access);

   Protocol_Error : exception;

private

   type HTTP_Filter_Factory is new Factory with null record;

   type HTTP_State is (Start_Line, Header, Entity);
   --  An HTTP session is either expecting a (request or response)
   --  message start line, a generic message header, or an entity
   --  constituting a message body?

   type HTTP_Version is record
     Major : Natural;
     Minor : Natural;
   end record;

   type HTTP_Filter is new Filter with record
      Role : PolyORB.ORB.Endpoint_Role;
      --  The role associated with this protocol engine.

      State  : HTTP_State;
      --  Current state of the HTTP session.

      CR_Seen : Boolean := False;
      --  In Start_Line or Header state, True iff the last character
      --  seen is a CR.

      In_Buf : PolyORB.Buffers.Buffer_Access;
      Data_Received : Ada.Streams.Stream_Element_Count;
      --  Data received in In_Buf and not processed yet
      --  (reset when changing states).

      --  Protocol parameters

      Version : HTTP_Version;
      Request_Method : PolyORB.HTTP_Methods.Method;
      Request_URI : PolyORB.Types.String;
      Status : Natural;
   end record;

   function Handle_Message
     (F : access HTTP_Filter;
      S : Components.Message'Class)
     return Components.Message'Class;

end PolyORB.Filters.HTTP;
