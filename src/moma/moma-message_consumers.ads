------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               M O M A . M E S S A G E _ C O N S U M E R S                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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

--  A Message_Consumer object is the client view of the message receiving
--  process. It is the fa�ade to all communications carried out with
--  a message pool to receive messages; it contains the stub to access
--  'Message_Consumer' servants (see MOMA.Provider and child packages
--  for more details).

--  NOTE: A MOMA client must use only this package, and its child packages to
--  receive messages from a message pool.

--  $Id$

with MOMA.Destinations;
with PolyORB.References;

package MOMA.Message_Consumers is

   type Message_Consumer is abstract tagged private;
   --  Destination : origin of all messages received.
   --  Ref : reference.
   --  (XXX to be defined, connection reference, pool reference ?)

   procedure Close;
   --  XXX not implemented. Rename it to Destroy ?

   function Get_Message_Selector return String;
   --  XXX not implemented.

   --  Accessors to Message_Consumer internal data.

   function Get_Ref (Self : Message_Consumer) return PolyORB.References.Ref;

   procedure Set_Ref (Self : in out Message_Consumer;
                      Ref  : PolyORB.References.Ref);

   function Get_Destination (Self : Message_Consumer)
                             return MOMA.Destinations.Destination;

   procedure Set_Destination (Self : in out Message_Consumer'Class;
                              Dest : MOMA.Destinations.Destination);

private
   type Message_Consumer is abstract tagged record
      Destination    : MOMA.Destinations.Destination;
      Ref            : PolyORB.References.Ref;
   end record;

end MOMA.Message_Consumers;
