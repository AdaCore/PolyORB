------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 R T P O R T A B L E S E R V E R . P O A                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Errors;
with PolyORB.ORB;
with PolyORB.POA_Manager;
with PolyORB.POA_Types;
with PolyORB.References;
with PolyORB.RT_POA;
with PolyORB.Setup;
with PolyORB.Smart_Pointers;
with PolyORB.Tasking.Priorities;

with PolyORB.CORBA_P.Exceptions;
with PolyORB.RTCORBA_P.To_ORB_Priority;

package body RTPortableServer.POA is

   use PolyORB.Tasking.Priorities;

   function To_RT_POA
     (Self : Local_Ref)
     return PolyORB.RT_POA.RT_Obj_Adapter_Access;
   --  Convert a Ref to a CORBA RTPOA to a PolyORB RTPOA

   ---------------
   -- To_RT_POA --
   ---------------

   function To_RT_POA
     (Self : Local_Ref)
     return PolyORB.RT_POA.RT_Obj_Adapter_Access
   is
      use PolyORB.Smart_Pointers;

      Res : constant PolyORB.Smart_Pointers.Entity_Ptr := Entity_Of (Self);

   begin
      if Res = null
        or else Res.all not in PolyORB.RT_POA.RT_Obj_Adapter'Class
      then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      declare
         use PolyORB.POA_Manager;

         The_POA : constant PolyORB.RT_POA.RT_Obj_Adapter_Access
           := PolyORB.RT_POA.RT_Obj_Adapter_Access (Res);
      begin
         if Is_Nil (The_POA.POA_Manager) then
            CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
         end if;

         return The_POA;
      end;
   end To_RT_POA;

   ------------------------------------
   -- Create_Reference_With_Priority --
   ------------------------------------

   function Create_Reference_With_Priority
     (Self      : Local_Ref;
      Intf      : CORBA.RepositoryId;
      Priority  : RTCORBA.Priority)
     return CORBA.Object.Ref
   is
      use PolyORB.Errors;
      use PolyORB.RT_POA;

      Error : PolyORB.Errors.Error_Container;

      RT_POA : constant PolyORB.RT_POA.RT_Obj_Adapter_Access
        := To_RT_POA (Self);

      U_Oid : PolyORB.POA_Types.Unmarshalled_Oid;

   begin
      PolyORB.RT_POA.Create_Object_Identification_With_Priority
        (RT_POA,
         null,
         PolyORB.RTCORBA_P.To_ORB_Priority (Priority),
         External_Priority (Priority),
         U_Oid,
         Error);

      if Found (Error) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end if;

      declare
         Oid : aliased PolyORB.POA_Types.Object_Id :=
           PolyORB.POA_Types.U_Oid_To_Oid (U_Oid);
         Result : PolyORB.References.Ref;
      begin
         --  Obtain object reference

         PolyORB.ORB.Create_Reference
           (PolyORB.Setup.The_ORB,
            Oid'Access,
            CORBA.To_Standard_String (Intf),
            Result);

         return CORBA.Object.Internals.To_CORBA_Ref (Result);
      end;
   end Create_Reference_With_Priority;

   -------------------------------------------
   -- Create_Reference_With_Id_And_Priority --
   -------------------------------------------

   function Create_Reference_With_Id_And_Priority
     (Self      : Local_Ref;
      Oid       : PortableServer.ObjectId;
      Intf      : CORBA.RepositoryId;
      Priority  : RTCORBA.Priority)
     return CORBA.Object.Ref
   is
      use PolyORB.Errors;
      use PolyORB.POA_Types;

      Error : PolyORB.Errors.Error_Container;

      RT_POA : constant PolyORB.RT_POA.RT_Obj_Adapter_Access :=
        To_RT_POA (Self);

      U_Oid : PolyORB.POA_Types.Unmarshalled_Oid;

      OOid : Object_Id_Access
        := new Object_Id'
        (PortableServer.Internals.To_PolyORB_Object_Id (Oid));

   begin
      PolyORB.RT_POA.Create_Object_Identification_With_Priority
        (RT_POA,
         OOid,
         PolyORB.RTCORBA_P.To_ORB_Priority (Priority),
         External_Priority (Priority),
         U_Oid,
         Error);

      Free (OOid);

      if Found (Error) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end if;

      declare
         A_Oid : aliased PolyORB.POA_Types.Object_Id :=
           PolyORB.POA_Types.U_Oid_To_Oid (U_Oid);
         Result : PolyORB.References.Ref;
      begin
         --  Obtain object reference

         PolyORB.ORB.Create_Reference
           (PolyORB.Setup.The_ORB,
            A_Oid'Access,
            CORBA.To_Standard_String (Intf),
            Result);

         return CORBA.Object.Internals.To_CORBA_Ref (Result);
      end;
   end Create_Reference_With_Id_And_Priority;

   -----------------------------------
   -- Activate_Object_With_Priority --
   -----------------------------------

   function Activate_Object_With_Priority
     (Self       : Local_Ref;
      P_Servant  : PortableServer.Servant;
      Priority   : RTCORBA.Priority)
     return PortableServer.ObjectId
   is
      use PortableServer;
      use PolyORB.Errors;

      Error : PolyORB.Errors.Error_Container;

      RT_POA : constant PolyORB.RT_POA.RT_Obj_Adapter_Access
        := To_RT_POA (Self);

      U_Oid : PolyORB.POA_Types.Unmarshalled_Oid;

   begin
      PolyORB.RT_POA.Activate_Object_With_Id_And_Priority
        (RT_POA,
         To_PolyORB_Servant (P_Servant),
         null,
         PolyORB.RTCORBA_P.To_ORB_Priority (Priority),
         External_Priority (Priority),
         U_Oid,
         Error);

      if Found (Error) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end if;

      declare
         Oid : constant PolyORB.POA_Types.Object_Id :=
           PolyORB.POA_Types.U_Oid_To_Oid (U_Oid);
      begin
         return PortableServer.Internals.To_PortableServer_ObjectId (Oid);
      end;
   end Activate_Object_With_Priority;

   ------------------------------------------
   -- Activate_Object_With_Id_And_Priority --
   ------------------------------------------

   procedure Activate_Object_With_Id_And_Priority
     (Self      : Local_Ref;
      Oid       : PortableServer.ObjectId;
      P_Servant : PortableServer.Servant;
      Priority  : RTCORBA.Priority)
   is
      use PortableServer;
      use PolyORB.Errors;

      Error : PolyORB.Errors.Error_Container;

      RT_POA : constant PolyORB.RT_POA.RT_Obj_Adapter_Access
        := To_RT_POA (Self);

      U_Oid : PolyORB.POA_Types.Unmarshalled_Oid;

      A_Oid : aliased PolyORB.POA_Types.Object_Id :=
        PortableServer.Internals.To_PolyORB_Object_Id (Oid);

   begin
      PolyORB.RT_POA.Activate_Object_With_Id_And_Priority
        (RT_POA,
         To_PolyORB_Servant (P_Servant),
         A_Oid'Unchecked_Access,
         PolyORB.RTCORBA_P.To_ORB_Priority (Priority),
         External_Priority (Priority),
         U_Oid,
         Error);

      if Found (Error) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end if;
   end Activate_Object_With_Id_And_Priority;

end RTPortableServer.POA;
