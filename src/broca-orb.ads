------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                            B R O C A . O R B                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with CORBA;
with CORBA.ORB;
with CORBA.Object;

with Broca.POA;
with Broca.Buffers;

pragma Elaborate_All (CORBA);

package Broca.ORB is

   pragma Elaborate_Body;

   procedure IOR_To_Object
     (IOR : access Broca.Buffers.Buffer_Type;
      Ref : out CORBA.Object.Ref'Class);

   function List_Initial_Services
     return CORBA.ORB.ObjectIdList;

   function Resolve_Initial_References
     (Identifier : CORBA.ORB.ObjectId)
     return CORBA.Object.Ref'Class;

   procedure Register_Initial_Reference
     (Identifier : in CORBA.ORB.ObjectId;
      Reference  : in CORBA.Object.Ref'Class);

   type ORB_Type is abstract tagged null record;
   --  Internal tricks to avoid to a client to contain any part of the
   --  server. The server part must register itself (only one server
   --  is allowed).

   procedure Run (ORB : in out ORB_Type) is abstract;

   --  A state of a poa has changed.
   procedure POA_State_Changed
     (ORB : in out ORB_Type;
      POA : in Broca.POA.Ref) is abstract;

   type ORB_Ptr is access all ORB_Type'Class;

   procedure Register_ORB (ORB : ORB_Ptr);
   procedure Run;
   procedure POA_State_Changed (POA : Broca.POA.Ref);

   --  Well Known ObjectIds.
   Root_POA_ObjectId               : constant CORBA.ORB.ObjectId;
   POA_Current_Objectid            : constant CORBA.ORB.ObjectId;
   Interface_Repository_ObjectId   : constant CORBA.ORB.ObjectId;
   Name_Service_ObjectId           : constant CORBA.ORB.ObjectId;

private

   Root_POA_ObjectId               : constant CORBA.ORB.ObjectId :=
     CORBA.ORB.To_CORBA_String ("RootPOA");

   POA_Current_Objectid            : constant CORBA.ORB.ObjectId :=
     CORBA.ORB.To_CORBA_String ("POACurrent");

   Interface_Repository_Objectid   : constant CORBA.ORB.ObjectId :=
     CORBA.ORB.To_CORBA_String ("InterfaceRepository");

   Name_Service_ObjectId           : constant CORBA.ORB.ObjectId :=
     CORBA.ORB.To_CORBA_String ("NamingService");

end Broca.ORB;
