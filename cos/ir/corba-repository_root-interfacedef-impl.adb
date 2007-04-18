------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 CORBA.REPOSITORY_ROOT.INTERFACEDEF.IMPL                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2007, Free Software Foundation, Inc.          --
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

with CORBA.ORB;
with PortableServer;

with CORBA.Repository_Root.Helper;
with CORBA.Repository_Root.OperationDef.Impl;
with CORBA.Repository_Root.AttributeDef.Impl;
with CORBA.Repository_Root.InterfaceDef.Skel;
pragma Warnings (Off, CORBA.Repository_Root.InterfaceDef.Skel);

with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);
with PolyORB.CORBA_P.Server_Tools;

package body CORBA.Repository_Root.InterfaceDef.Impl is

   -----------
   -- Debug --
   -----------

   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("interfacedef.impl");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   package L2 is new PolyORB.Log.Facility_Log ("interfacedef.impl_method_trace");
   procedure O2 (Message : Standard.String; Level : Log_Level := Debug)
     renames L2.Output;
   function C2 (Level : Log_Level := Debug) return Boolean
     renames L2.Enabled;
   pragma Unreferenced (C2); --  For conditional pragma Debug

   package IntDef renames IDL_SEQUENCE_CORBA_InterfaceDef_Forward;
   package IdSeq renames IDL_SEQUENCE_CORBA_RepositoryId;

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
                   Contents :
                     CORBA.Repository_Root.Contained.Impl.Contained_Seq.Sequence;
                   Contained_View :  CORBA.Repository_Root.Contained.Impl.Object_Ptr;
                   IDLType_View : CORBA.Repository_Root.IDLType.Impl.Object_Ptr;
                   Base_Interfaces : CORBA.Repository_Root.InterfaceDefSeq;
                   Is_Abstract : CORBA.Boolean) is
   begin
      pragma Debug (O2 ("init enter"));
      Container.Impl.Init (Container.Impl.Object_Ptr (Self),
                           Real_Object,
                           Def_Kind,
                           Contents);
      pragma Debug (O ("init : before contained init"));
      Contained.Impl.Init (Contained_View,
                            Real_Object,
                           Def_Kind,
                           Id,
                           Name,
                           Version,
                           Defined_In);
      pragma Debug (O2 ("init : before idltype init"));
      IDLType.Impl.Init (IDLType_View,
                         Real_Object,
                         Def_Kind);
      pragma Debug (O2 ("init : after idltype init"));
      Self.Contained_View := Contained_View;
      Self.IDLType_View := IDLType_View;
      Self.Is_Abstract := Is_Abstract;
      Self.Base_Interfaces := Base_Interfaces;
      pragma Debug (O2 ("init  end"));
   end Init;

   -----------------
   --  To_Object  --
    -----------------
   function To_Object (Fw_Ref : InterfaceDef_Forward.Ref)
                       return Object_Ptr is
      Result : Portableserver.Servant;
   begin
      PolyORB.CORBA_P.Server_Tools.Reference_To_Servant
        (InterfaceDef.Convert_Forward.To_Ref (Fw_Ref),
         Result);
      return Object_Ptr (Result);
   end To_Object;

   ------------------
   --  To_Forward  --
   ------------------
   function To_Forward (Obj : Object_Ptr)
                        return InterfaceDef_Forward.Ref is
      Ref : InterfaceDef.Ref;
   begin
      PolyORB.CORBA_P.Server_Tools.Initiate_Servant (PortableServer.Servant (Obj),
                                           Ref);
      return InterfaceDef.Convert_Forward.To_Forward (Ref);
   end To_Forward;

   ---------------------------------
   --  To get the secondary views --
   ---------------------------------

   function Get_Contained_View (Self : access Object)
     return CORBA.Repository_Root.Contained.Impl.Object_Ptr is
   begin
      return Self.Contained_View;
   end Get_Contained_View;

   function Get_IDLType_View (Self : access Object)
     return CORBA.Repository_Root.IDLType.Impl.Object_Ptr is
   begin
      return Self.IDLType_View;
   end Get_IDLType_View;

   function get_base_interfaces
     (Self : access Object)
     return CORBA.Repository_Root.InterfaceDefSeq
   is
   begin
      return Self.Base_Interfaces;
   end get_base_interfaces;

   procedure set_base_interfaces
     (Self : access Object;
      To : CORBA.Repository_Root.InterfaceDefSeq) is
   begin
      Self.Base_Interfaces := To;
   end set_base_interfaces;

   function get_is_abstract
     (Self : access Object)
     return CORBA.Boolean
   is
   begin
      return Self.Is_Abstract;
   end get_is_abstract;

   procedure set_is_abstract
     (Self : access Object;
      To : CORBA.Boolean) is
   begin
      Self.Is_Abstract := To;
   end set_is_abstract;

   function is_a
     (Self : access Object;
      interface_id : CORBA.RepositoryId)
     return CORBA.Boolean
   is
      Result : CORBA.Boolean;
   begin
      --  Insert implementation of is_a
      raise Program_Error;
      return Result;
   end is_a;

   function describe_interface
     (Self : access Object)
     return CORBA.Repository_Root.InterfaceDef.FullInterfaceDescription
   is
      pragma Unreferenced (Self);

      Result : CORBA.Repository_Root.InterfaceDef.FullInterfaceDescription;
      pragma Warnings (Off, Result);
      --  Dummy value, use default initialization
   begin

      --  Insert implementation of describe_interface
      --  XXX not implemented yet

      raise Program_Error;
      return Result;
   end describe_interface;

   function create_attribute
     (Self : access Object;
      id : CORBA.RepositoryId;
      name : CORBA.Identifier;
      version : CORBA.Repository_Root.VersionSpec;
      IDL_type : CORBA.Repository_Root.IDLType.Ref;
      mode : CORBA.Repository_Root.AttributeMode)
     return CORBA.Repository_Root.AttributeDef.Ref
   is
   begin
      Check_Structure (Self, Dk_Attribute);
      Check_Id (Self, Id);
      Check_Name (Self, Name);

      declare
         Result : CORBA.Repository_Root.AttributeDef.Ref;
         Obj : constant AttributeDef.Impl.Object_Ptr := new AttributeDef.Impl.Object;
      begin
         --  initialization of the object
         AttributeDef.Impl.Init (Obj,
                                 IRObject.Impl.Object_Ptr (Obj),
                                 Dk_Attribute,
                                 Id,
                                 Name,
                                 Version,
                                 Container.Impl.To_Forward
                                 (Container.Impl.Object_Ptr (Self)),
                                 IDL_Type,
                                 Mode);

         --  add it to the contents field of this container
         Container.Impl.Append_To_Contents
           (Container.Impl.Object_Ptr (Self),
            Contained.Impl.To_Contained (IRObject.Impl.Object_Ptr (Obj)));

         --  activate it
         PolyORB.CORBA_P.Server_Tools.Initiate_Servant (PortableServer.Servant (Obj),
                                              Result);
         return Result;
      end;
   end create_attribute;

   function create_operation
     (Self       : access Object;
      id         : CORBA.RepositoryId;
      name       : CORBA.Identifier;
      version    : CORBA.Repository_Root.VersionSpec;
      IDL_result : CORBA.Repository_Root.IDLType.Ref;
      mode       : CORBA.Repository_Root.OperationMode;
      params     : CORBA.Repository_Root.ParDescriptionSeq;
      exceptions : CORBA.Repository_Root.ExceptionDefSeq;
      contexts   : CORBA.Repository_Root.ContextIdSeq)
     return CORBA.Repository_Root.OperationDef.Ref
   is
   begin
      Check_Structure (Self, Dk_Operation);
      Check_Id (Self, Id);
      Check_Name (Self, Name);

      declare
         Result : CORBA.Repository_Root.OperationDef.Ref;
         Obj : constant OperationDef.Impl.Object_Ptr
           := new OperationDef.Impl.Object;
      begin
         --  initialization of the object
         OperationDef.Impl.Init (Obj,
                                 IRObject.Impl.Object_Ptr (Obj),
                                 Dk_Operation,
                                 Id,
                                 Name,
                                 Version,
                                 Container.Impl.To_Forward
                                 (Container.Impl.Object_Ptr (Self)),
                                 IDL_Result,
                                 Params,
                                 Mode,
                                 Contexts,
                                 Exceptions);

         --  add it to the contents field of this container
         Container.Impl.Append_To_Contents
           (Container.Impl.Object_Ptr (Self),
            Contained.Impl.To_Contained (IRObject.Impl.Object_Ptr (Obj)));

         --  activate it
         PolyORB.CORBA_P.Server_Tools.Initiate_Servant (PortableServer.Servant (Obj),
                                              Result);
         return Result;
      end;
   end create_operation;

   --------------------------------
   --  inherited from contained  --
   --------------------------------
   function get_id
     (Self : access Object)
     return CORBA.RepositoryId
   is
   begin
      return Contained.Impl.Get_Id (Self.Contained_View);
   end get_id;

   procedure set_id
     (Self : access Object;
      To : CORBA.RepositoryId) is
   begin
      Contained.Impl.Set_Id (Self.Contained_View, To);
   end set_id;

   function get_name
     (Self : access Object)
     return CORBA.Identifier
   is
   begin
      return Contained.Impl.Get_Name (Self.Contained_View);
   end get_name;

   procedure set_name
     (Self : access Object;
      To : CORBA.Identifier) is
   begin
      Contained.Impl.Set_Name (Self.Contained_View, To);
   end set_name;

   function get_version
     (Self : access Object)
     return CORBA.Repository_Root.VersionSpec
   is
   begin
      return Contained.Impl.Get_Version (Self.Contained_View);
   end get_version;

   procedure set_version
     (Self : access Object;
      To : CORBA.Repository_Root.VersionSpec) is
   begin
      Contained.Impl.Set_Version (Self.Contained_View, To);
   end set_version;

   function get_defined_in
     (Self : access Object)
     return CORBA.Repository_Root.Container_Forward.Ref
   is
   begin
       return Contained.Impl.Get_Defined_In (Self.Contained_View);
   end get_defined_in;

   function get_absolute_name
     (Self : access Object)
      return CORBA.ScopedName
   is
   begin
      return Contained.Impl.Get_Absolute_Name (Self.Contained_View);
   end get_absolute_name;

   function get_containing_repository
     (Self : access Object)
     return CORBA.Repository_Root.Repository_Forward.Ref
   is
   begin
      return Contained.Impl.Get_Containing_Repository (Self.Contained_View);
   end get_containing_repository;

   function describe
     (Self : access Object)
     return CORBA.Repository_Root.Contained.Description
   is
      Result : CORBA.Repository_Root.Contained.Description;
      Desc : CORBA.Repository_Root.InterfaceDescription;
   begin
      Desc := (Name => Get_Name (Self),
               Id => Get_Id (Self),
               Defined_In => Contained.Impl.Get_Defined_In
               (Self.Contained_View),
               Version => Get_Version (Self),
               Base_Interfaces => Get_RepositoryIdSeq (Self.Base_Interfaces),
               Is_Abstract => Self.Is_Abstract);
      Result := (Kind => Get_Def_Kind (Self),
                 Value => CORBA.Repository_Root.Helper.To_Any (Desc));
      return Result;
   end describe;

   procedure move
     (Self : access Object;
      new_container : CORBA.Repository_Root.Container_Forward.Ref;
      new_name : CORBA.Identifier;
      new_version : CORBA.Repository_Root.VersionSpec) is
   begin
      Contained.Impl.Move (Self.Contained_View,
                           New_Container,
                           New_Name,
                           New_Version);
   end move;

   ----------------------------
   -- Inherited from IDLType --
   ----------------------------

   function get_type (Self : access Object) return CORBA.TypeCode.Object is
   begin
      return CORBA.ORB.Create_Interface_Tc (Get_Id (Self), Get_Name (Self));
   end get_type;

   -------------------------
   -- Get_RepositoryIdSeq --
   -------------------------

   function Get_RepositoryIdSeq
     (IntDefSeq : InterfaceDefSeq) return RepositoryIdSeq
   is
      Result    : RepositoryIdSeq;
      Int_Array : IntDef.Element_Array :=
                    IntDef.To_Element_Array (IntDef.Sequence (IntDefSeq));
   begin
      for I in Int_Array'Range loop
         declare
            Int : constant Object_Ptr := To_Object (Int_Array (I));
         begin
            IdSeq.Append (IdSeq.Sequence (Result), Get_Id (Int));
         end;
      end loop;
      return Result;
   end Get_RepositoryIdSeq;

end CORBA.Repository_Root.InterfaceDef.Impl;
