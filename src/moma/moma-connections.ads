------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                     M O M A . C O N N E C T I O N S                      --
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

--  $Id$

with MOMA.Message_Consumers;
with MOMA.Types;
with PolyORB.References;

package MOMA.Connections is

   type Connection is abstract tagged private;

   procedure Close;

   function Get_Client_Id (Self : Connection) return MOMA.Types.String;

   procedure Set_Client_Id (Self : in out Connection;
                            Client_Id : MOMA.Types.String);

   function Get_Ref (Self : Connection) return PolyORB.References.Ref;

   procedure Set_Ref (Self : in out Connection;
                      Ref  : in PolyORB.References.Ref);

   procedure Start;

   procedure Stop;

   function Get_Meta_Data return MOMA.Types.Meta_Data;


   function Create_Consumer return Message_Consumers.Message_Consumer
      is abstract;
   --  XXX should REALLY be there ? check JMS ...

   ---------------------------------------
   -- Abstract Create_Session Function --
   ---------------------------------------

   --  function Create_Session (Self : Connection;
   --                         Transacted : Boolean;
   --                         Ackowledge_Mode : Acknowledge_Type)
   --                        return Sessions.Session
   --  is abstract;


private
   type Connection is abstract tagged record
      Client_Id : MOMA.Types.String;
      Ref : PolyORB.References.Ref;
   end record;

end MOMA.Connections;
