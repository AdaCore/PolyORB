------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   C O R B A . R E P O S I T O R Y _ R O O T . E N U M D E F . I M P L    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2012, Free Software Foundation, Inc.          --
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

pragma Style_Checks (Off);

with CORBA.ORB.TypeCode;
with PolyORB.CORBA_P.Server_Tools;
with PortableServer;

with CORBA.Repository_Root.EnumDef.Skel;
pragma Warnings (Off, CORBA.Repository_Root.EnumDef.Skel);

package body CORBA.Repository_Root.EnumDef.Impl is

   -----------------
   --  To_Object  --
   -----------------
   function To_Object (Fw_Ref : EnumDef_Forward.Ref)
     return Object_Ptr is
      Result : Portableserver.Servant;
   begin
      PolyORB.CORBA_P.Server_Tools.Reference_To_Servant
        (EnumDef.Convert_Forward.To_Ref (Fw_Ref),
         Result);
      return Object_Ptr (Result);
   end To_Object;

   ------------------
   --  To_Forward  --
   ------------------
   function To_Forward (Obj : Object_Ptr)
                        return EnumDef_Forward.Ref is
      Ref : EnumDef.Ref;
   begin
      PolyORB.CORBA_P.Server_Tools.Initiate_Servant (PortableServer.Servant (Obj),
                                           Ref);
      return EnumDef.Convert_Forward.To_Forward (Ref);
   end To_Forward;

   ------------
   --  INIT  --
   ------------
   procedure Init (Self : access Object;
                   Real_Object :
                     CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
                   Def_Kind : CORBA.Repository_Root.DefinitionKind;
                   Id : CORBA.RepositoryId;
                   Name : CORBA.Identifier;
                   Version : CORBA.Repository_Root.VersionSpec;
                   Defined_In : CORBA.Repository_Root.Container_Forward.Ref;
                   IDLType_View : CORBA.Repository_Root.IDLType.Impl.Object_Ptr;
                   Members : CORBA.Repository_Root.EnumMemberSeq) is
   begin
      Typedefdef.Impl.Init (Typedefdef.Impl.Object_Ptr (Self),
                            Real_Object,
                            Def_Kind,
                            Id,
                            Name,
                            Version,
                            Defined_In,
                            IDLType_View);
      Self.Members := Members;
   end Init;

   ----------------
   --  get_type  --
   ----------------
   function get_type
     (Self : access Object)
      return CORBA.TypeCode.Object
   is
   begin
      return CORBA.ORB.TypeCode.Create_Enum_Tc
        (Get_Id (Self), Get_Name (Self), Self.Members);
   end get_type;

   function get_members
     (Self : access Object)
     return CORBA.Repository_Root.EnumMemberSeq
   is
   begin
      return Self.Members;
   end get_members;

   procedure set_members
     (Self : access Object;
      To : CORBA.Repository_Root.EnumMemberSeq) is
   begin
      Self.Members := To;
   end set_members;

end CORBA.Repository_Root.EnumDef.Impl;
