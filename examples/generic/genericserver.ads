------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                        G E N E R I C S E R V E R                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 1999 ENST Paris University, France.             --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with CORBA.ORB; use CORBA.ORB;
with CORBA.Object;
with PortableServer;
with PortableServer.POA;
with PortableServer.ServantManager;
pragma Elaborate_All (CORBA.Object, PortableServer.ServantManager);

package GenericServer is
   type String_Acc is access String;

   --  Flags
   Flag_Verbose : Boolean := False;
   Flag_Discard : Boolean := False;
   Flag_Tilt : Boolean := False;
   Flag_Servant_Policy : PortableServer.RequestProcessingPolicyValue :=
     PortableServer.USE_ACTIVE_OBJECT_MAP_ONLY;
   Flag_Ior_File : String_Acc := null;

   --  If set, there is a wall to answer your request
   Flag_Wall : Boolean := True;


   --  If True, the POA managing the servant has single_thread policy.
   Flag_Serial : Boolean := False;

   Bad_Option : exception;
   My_Obj : PortableServer.Servant;
   My_Servant_Manager : PortableServer.ServantManager.Ref;
   Ref_To_Export : CORBA.Object.Ref;
   Repository_Id : String_Acc;

   procedure Decode_Options;

   function Build_Poa_Tree (Poa : PortableServer.POA.Ref)
     return PortableServer.POA.Ref;

   procedure Main (Obj : in PortableServer.Servant);
   
end GenericServer;
