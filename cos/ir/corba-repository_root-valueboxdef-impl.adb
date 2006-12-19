------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 CORBA.REPOSITORY_ROOT.VALUEBOXDEF.IMPL                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2006, Free Software Foundation, Inc.             --
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

pragma Style_Checks (Off);

with CORBA.ORB.TypeCode;
with PortableServer;

with CORBA.Repository_Root.ValueBoxDef.Skel;
pragma Warnings (Off, CORBA.Repository_Root.ValueBoxDef.Skel);

with PolyORB.CORBA_P.Server_Tools;

package body CORBA.Repository_Root.ValueBoxDef.Impl is

   -----------------
   --  To_Object  --
   -----------------
   function To_Object (Fw_Ref : ValueBoxDef_Forward.Ref)
                       return Object_Ptr is
      Result : Portableserver.Servant;
   begin
      PolyORB.CORBA_P.Server_Tools.Reference_To_Servant
        (ValueBoxDef.Convert_Forward.To_Ref (Fw_Ref),
         Result);
      return ValueBoxDef.Impl.Object_Ptr (Result);
   end To_Object;

   ------------------
   --  To_Forward  --
   ------------------
   function To_Forward (Obj : Object_Ptr)
                        return ValueBoxDef_Forward.Ref is
      Ref : ValueBoxDef.Ref;
   begin
      PolyORB.CORBA_P.Server_Tools.Initiate_Servant
        (PortableServer.Servant (Obj), Ref);
      return ValueBoxDef.Convert_Forward.To_Forward (Ref);
   end To_Forward;

   ------------
   --  INIT  --
   ------------
   procedure Init
     (Self : access Object;
      Real_Object : CORBA.Repository_Root.IRObject.Impl.Object_Ptr;
      Def_Kind : CORBA.Repository_Root.DefinitionKind;
      Id : CORBA.RepositoryId;
      Name : CORBA.Identifier;
      Version : CORBA.Repository_Root.VersionSpec;
      Defined_In : CORBA.Repository_Root.Container_Forward.Ref;
      IDLType_View : CORBA.Repository_Root.IDLType.Impl.Object_Ptr;
      Original_Type_Def : CORBA.Repository_Root.IDLType.Ref) is
   begin
      Typedefdef.Impl.Init (Typedefdef.Impl.Object_Ptr (Self),
                            Real_Object,
                            Def_Kind,
                            Id,
                            Name,
                            Version,
                            Defined_In,
                            IDLType_View);
      Self.Original_Type_Def := Original_Type_Def;
   end Init;

   ----------------
   --  get_type  --
   ----------------
   function get_type
     (Self : access Object)
      return CORBA.TypeCode.Object
   is
      Obj : Portableserver.Servant;
      Orig_TC : CORBA.TypeCode.Object;
   begin
      PolyORB.CORBA_P.Server_Tools.Reference_To_Servant (Self.Original_Type_Def,
                                               Obj);
      Orig_TC := IDLType.Impl.Get_Type
        (IDLType.Impl.To_IDLType
         (IRObject.Impl.Object_Ptr (Obj)));

      return CORBA.ORB.TypeCode.Create_Value_Box_Tc
        (Get_Id (Self), Get_Name (Self), Orig_TC);
   end get_type;

   function get_original_type_def
     (Self : access Object)
     return CORBA.Repository_Root.IDLType.Ref
   is
   begin
      return Self.Original_Type_Def;
   end get_original_type_def;

   procedure set_original_type_def
     (Self : access Object;
      To : CORBA.Repository_Root.IDLType.Ref) is
   begin
      Self.Original_Type_Def := To;
   end set_original_type_def;

end CORBA.Repository_Root.ValueBoxDef.Impl;
