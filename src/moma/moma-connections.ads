------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     M O M A . C O N N E C T I O N S                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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
