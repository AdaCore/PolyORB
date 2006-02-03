------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 CORBA.REPOSITORY_ROOT.CONSTANTDEF.IMPL                   --
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

with PortableServer;

with CORBA.Repository_Root.IDLType.Impl;
with CORBA.Repository_Root.Helper;
with CORBA.Repository_Root.ConstantDef.Skel;
pragma Warnings (Off, CORBA.Repository_Root.ConstantDef.Skel);

with PolyORB.CORBA_P.Server_Tools;

package body CORBA.Repository_Root.ConstantDef.Impl is

   -----------------
   --  To_Object  --
   -----------------
   function To_Object (Fw_Ref : ConstantDef_Forward.Ref)
     return Object_Ptr is
      Result : Portableserver.Servant;
   begin
      PolyORB.CORBA_P.Server_Tools.Reference_To_Servant
        (ConstantDef.Convert_Forward.To_Ref (Fw_Ref),
         Result);
      return Object_Ptr (Result);
   end To_Object;

   ------------------
   --  To_Forward  --
   ------------------
   function To_Forward (Obj : Object_Ptr)
                        return ConstantDef_Forward.Ref is
      Ref : ConstantDef.Ref;
   begin
      PolyORB.CORBA_P.Server_Tools.Initiate_Servant (PortableServer.Servant (Obj),
                                           Ref);
      return ConstantDef.Convert_Forward.To_Forward (Ref);
   end To_Forward;

   ----------------------
   --  Procedure init  --
   ----------------------
   procedure Init (Self : access Object;
                   Real_Object : IRObject.Impl.Object_Ptr;
                   Def_Kind : CORBA.Repository_Root.DefinitionKind;
                   Id : CORBA.RepositoryId;
                   Name : CORBA.Identifier;
                   Version : CORBA.Repository_Root.VersionSpec;
                   Defined_In : CORBA.Repository_Root.Container_Forward.Ref;
                   Type_Def : CORBA.Repository_Root.IDLType.Ref;
                   Value : CORBA.Any) is
   begin
      Contained.Impl.Init (Contained.Impl.Object_Ptr(Self),
                           Real_Object,
                           Def_Kind,
                           Id,
                           Name,
                           Version,
                           Defined_In);
      Self.Type_Def := Type_Def;
      Set_Value (Self, Value);
   end Init;

   function get_type
     (Self : access Object)
     return CORBA.TypeCode.Object
   is
      Obj : Portableserver.Servant;
   begin
      PolyORB.CORBA_P.Server_Tools.Reference_To_Servant (Self.Type_Def,
                                               Obj);
      --  The type should be the type of the Type_def
      return IDLType.Impl.Get_Type
        (IDLType.Impl.To_IDLType
         (IRObject.Impl.Object_Ptr
          (Obj)));
   end get_type;

   function get_type_def
     (Self : access Object)
     return CORBA.Repository_Root.IDLType.Ref
   is
   begin
      return Self.Type_Def;
   end get_type_def;

   procedure set_type_def
     (Self : access Object;
      To : CORBA.Repository_Root.IDLType.Ref) is
   begin
      Self.Type_Def := To;
   end set_type_def;

   function get_value
     (Self : access Object)
     return CORBA.Any
   is
   begin
      return Self.Value;
   end get_value;

   procedure set_value
     (Self : access Object;
      To : CORBA.Any) is
      use CORBA.Typecode;
   begin
      if CORBA.Get_Type (Self.Value) = Get_Type (Self)
      then
         Self.Value := To;
      else
         CORBA.Raise_Bad_Param (CORBA.System_Exception_Members'
                                (Minor => 2,
                                 Completed => CORBA.Completed_No));
      end if;
   end set_value;

   ----------------
   --  Describe  --
   ----------------
   function describe
     (Self : access Object)
     return CORBA.Repository_Root.Contained.Description
     is
      Result : CORBA.Repository_Root.Contained.Description;
      Desc : CORBA.Repository_Root.ConstantDescription;
   begin
      Desc := (Name => Get_Name (Self),
               Id => Get_Id (Self),
               Defined_In => Get_Defined_In (Self),
               Version => Get_Version (Self),
               IDL_Type => Get_Type (Self),
               Value => Self.Value);
      Result := (Kind => Get_Def_Kind (Self),
                 Value => CORBA.Repository_Root.Helper.To_Any (Desc));
      return Result;
   end describe;

end CORBA.Repository_Root.ConstantDef.Impl;
