------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     M O M A . C O N N E C T I O N S                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  A Connection provides access to the provider for the client.

with MOMA.Connection_Factories;
with MOMA.Types;

package MOMA.Connections is

   type Connection is private;
   --  Client_Id : Id of the MOMA client.
   --  Ref       : Reference.

   procedure Close;
   --  Close the connection.

   function Create_Connection
     (Factory : MOMA.Connection_Factories.Connection_Factory)
      return Connection;
   --  Create a new connection using this connection factory.

   function Create_Connection
     (Factory   : MOMA.Connection_Factories.Connection_Factory;
      Username  : String;
      Password  : String)
      return Connection;
   --  Create a new connection using this connection factory
   --  and providing a username/password.
   --  XXX Not implemented.

   --  Accessors to Connection internal data.

   function Get_Client_Id
     (Self : Connection)
     return MOMA.Types.String;

   procedure Set_Client_Id
     (Self      : in out Connection;
      Client_Id :        MOMA.Types.String);

   function Get_Ref
     (Self : Connection)
     return MOMA.Types.Ref;

   procedure Set_Ref
     (Self : in out Connection;
      Ref  : MOMA.Types.Ref);

   procedure Start;
   --  Start the connection, i.e activate all rattached message producers
   --  and consumers.
   --  XXX to be implemented.

   procedure Stop;
   --  Stop the connection, i.e desactivate all rattached message producers
   --  and consumers.
   --  XXX to be implemented.

   --  XXX check the conformance and pertinence of the above spec.

   function Get_Meta_Data return MOMA.Types.Meta_Data;

private
   type Connection is record
      Client_Id  : MOMA.Types.String;
      Ref        : MOMA.Types.Ref;
   end record;

end MOMA.Connections;
