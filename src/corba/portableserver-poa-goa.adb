------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O R T A B L E S E R V E R . P O A . G O A                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Binding_Data.Local;

with PolyORB.Obj_Adapters.Group_Object_Adapter;

with PolyORB.POA;
with PolyORB.POA_Types;
with PolyORB.POA_Manager;
with PolyORB.References;
with PolyORB.Servants.Group_Servants;
with PolyORB.Smart_Pointers;

with PolyORB.CORBA_P.Exceptions;
with PolyORB.Errors;

package body PortableServer.POA.GOA is

   use PolyORB.Errors;

   function To_POA
     (Self : Ref)
     return PolyORB.POA.Obj_Adapter_Access;

   --  Group management

   procedure Associate
     (Group : PolyORB.Servants.Servant_Access;
      Oid   : PolyORB.Objects.Object_Id);
   --  Associate a group servant to an Oid

   procedure Disassociate
     (Group : PolyORB.Servants.Servant_Access;
      Oid   : PolyORB.Objects.Object_Id);
   --  Remove an Oid from a group servant

   ------------
   -- To_Ref --
   ------------

   function To_Ref
     (Self : CORBA.Object.Ref'Class)
     return Ref
   is
      Result : Ref;
   begin
      if CORBA.Object.Entity_Of (Self).all
        not in PolyORB.POA.Obj_Adapter'Class then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      Set (Result, CORBA.Object.Entity_Of (Self));
      return Result;
   end To_Ref;

   ------------
   -- To_POA --
   ------------

   function To_POA
     (Self : Ref)
     return PolyORB.POA.Obj_Adapter_Access
   is
      use PolyORB.Smart_Pointers;

      Res : constant PolyORB.Smart_Pointers.Entity_Ptr :=
        Entity_Of (Self);

   begin
      if Res = null
        or else Res.all not in PolyORB.POA.Obj_Adapter'Class then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      declare
         use PolyORB.POA_Manager;

         The_POA : constant PolyORB.POA.Obj_Adapter_Access :=
           PolyORB.POA.Obj_Adapter_Access (Res);
      begin
         if Is_Nil (The_POA.POA_Manager) then
            CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
         end if;

         return The_POA;
      end;
   end To_POA;

   ----------------------
   -- Raise_From_Error --
   ----------------------

   procedure Raise_From_Error
     (Error : in out PolyORB.Errors.Error_Container);

   procedure Raise_From_Error
     (Error : in out PolyORB.Errors.Error_Container) is
   begin
      pragma Assert (Is_Error (Error));

      case Error.Kind is
         when NotAGroupObject_E =>
            declare
               Member : constant NotAGroupObject_Members
                 := NotAGroupObject_Members'
                 (CORBA.IDL_Exception_Members with null record);
            begin
               Free (Error.Member);
               Raise_NotAGroupObject (Member);
            end;
         when others =>
            PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end case;
   end Raise_From_Error;

   ---------------
   -- Associate --
   ---------------

   procedure Associate
     (Group : PolyORB.Servants.Servant_Access;
      Oid   : PolyORB.Objects.Object_Id)
   is
      use PolyORB.Binding_Data;
      use PolyORB.Binding_Data.Local;

      P     : constant Profile_Access := new Local_Profile_Type;
      Ref   : PolyORB.References.Ref;
      Error : Error_Container;
   begin
      Create_Local_Profile (Oid, Local_Profile_Type (P.all));
      PolyORB.References.Create_Reference ((1 => P), "", Ref);
      PolyORB.Servants.Group_Servants.Associate (Group, Ref, Error);
      if Found (Error) then
         Raise_From_Error (Error);
      end if;
   end Associate;

   ------------------
   -- Disassociate --
   ------------------

   procedure Disassociate
     (Group : PolyORB.Servants.Servant_Access;
      Oid   : PolyORB.Objects.Object_Id)
   is
      use PolyORB.Servants.Group_Servants;

      It    : PolyORB.Servants.Group_Servants.Iterator;
      Error : Error_Container;
   begin
      First (Group, It, Error);
      if Found (Error) then
         Raise_From_Error (Error);
      end if;
      while not Last (It) loop
         declare
            use PolyORB.References;
            use PolyORB.Binding_Data;
            use PolyORB.Objects;

            Pro : constant Profile_Array
              := Profiles_Of (Value (It));
         begin
            for J in Pro'Range loop
               if Oid = Get_Object_Key (Pro (J).all).all then
                  Disassociate (Group, Value (It), Error);
                  if Found (Error) then
                     Raise_From_Error (Error);
                  end if;
                  return;
               end if;
            end loop;
         end;
         Next (It);
      end loop;
   end Disassociate;

   -----------------------------
   -- Create_Id_For_Reference --
   -----------------------------

   function Create_Id_For_Reference
     (Self    : Ref;
      The_Ref : CORBA.Object.Ref)
     return PortableServer.ObjectId
   is
      use PolyORB.POA;
      use PolyORB.POA_Types;
      use PolyORB.Servants;
      use PolyORB.Servants.Group_Servants;
      use PolyORB.Obj_Adapters.Group_Object_Adapter;

      U_Oid : Unmarshalled_Oid;
      Error : Error_Container;
      GS    : constant Servant_Access
        := Get_Group (CORBA.Object.Internals.To_PolyORB_Ref (The_Ref),
                      True);
   begin
      if GS = null then
         declare
            Member : constant NotAGroupObject_Members
              := NotAGroupObject_Members'
              (CORBA.IDL_Exception_Members with null record);
         begin
            Raise_NotAGroupObject (Member);
         end;
      end if;

      Create_Object_Identification (To_POA (Self), null, U_Oid, Error);
      if Found (Error) then
         Raise_From_Error (Error);
      end if;

      declare
         Oid : constant PolyORB.Objects.Object_Id
           := U_Oid_To_Oid (U_Oid);

      begin
         Associate (GS, Oid);
         return PortableServer.Internals.To_PortableServer_ObjectId (Oid);
      end;
   end Create_Id_For_Reference;

   ----------------------
   -- Reference_To_Ids --
   ----------------------

   function Reference_To_Ids
     (Self    : Ref;
      The_Ref : CORBA.Object.Ref)
     return IDs
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

      use Sequence_IDs;
      use PolyORB.Servants;
      use PolyORB.Servants.Group_Servants;
      use PolyORB.Obj_Adapters.Group_Object_Adapter;

      GS : constant Servant_Access
        := Get_Group (CORBA.Object.Internals.To_PolyORB_Ref (The_Ref));
   begin
      if GS = null then
         declare
            Member : constant NotAGroupObject_Members
              := NotAGroupObject_Members'
              (CORBA.IDL_Exception_Members with null record);
         begin
            Raise_NotAGroupObject (Member);
         end;
      end if;

      declare
         It    : PolyORB.Servants.Group_Servants.Iterator;
         List  : Sequence := Null_Sequence;
         Error : Error_Container;
      begin
         First (GS, It, Error);
         if Found (Error) then
            Raise_From_Error (Error);
         end if;
         while not Last (It) loop
            declare
               use PolyORB.References;
               use PolyORB.Binding_Data;
               use PolyORB.Objects;

               Pro : constant Profile_Array
                 := Profiles_Of (Value (It));
               Oid : Object_Id_Access;
            begin
               for J in Pro'Range loop
                  Oid := Get_Object_Key (Pro (J).all);
                  if Oid /= null then
                     Append (List,
                             PortableServer.Internals.
                             To_PortableServer_ObjectId (Oid.all));
                     exit;
                  end if;
               end loop;
            end;
            Next (It);
         end loop;
         return IDs (List);
      end;
   end Reference_To_Ids;

   ---------------------------------
   -- Associate_Reference_With_Id --
   ---------------------------------

   procedure Associate_Reference_With_Id
     (Self : Ref;
      Ref  : CORBA.Object.Ref;
      Oid  : PortableServer.ObjectId)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

      use PolyORB.Servants;
      use PolyORB.Servants.Group_Servants;
      use PolyORB.Obj_Adapters.Group_Object_Adapter;

      GS : constant Servant_Access
        := Get_Group (CORBA.Object.Internals.To_PolyORB_Ref (Ref), True);
   begin
      if GS = null then
         declare
            Member : constant NotAGroupObject_Members
              := NotAGroupObject_Members'
              (CORBA.IDL_Exception_Members with null record);
         begin
            Raise_NotAGroupObject (Member);
         end;
      end if;
      Associate (GS, PortableServer.Internals.To_PolyORB_Object_Id (Oid));
   end Associate_Reference_With_Id;

   ------------------------------------
   -- Disassociate_Reference_With_Id --
   ------------------------------------

   procedure Disassociate_Reference_With_Id
     (Self : Ref;
      Ref  : CORBA.Object.Ref;
      Oid  : PortableServer.ObjectId)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

      use PolyORB.Servants;
      use PolyORB.Servants.Group_Servants;
      use PolyORB.Obj_Adapters.Group_Object_Adapter;

      GS : constant Servant_Access
        := Get_Group (CORBA.Object.Internals.To_PolyORB_Ref (Ref));
   begin
      if GS = null then
         declare
            Member : constant NotAGroupObject_Members
              := NotAGroupObject_Members'
              (CORBA.IDL_Exception_Members with null record);
         begin
            Raise_NotAGroupObject (Member);
         end;
      end if;
      Disassociate (GS, PortableServer.Internals.To_PolyORB_Object_Id (Oid));
   end Disassociate_Reference_With_Id;

end PortableServer.POA.GOA;
