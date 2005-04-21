------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                           I O P . H E L P E R                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2005 Free Software Foundation, Inc.             --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.Utils.Strings;
with PolyORB.Initialization;
with PolyORB.Sequences.Unbounded.CORBA_Helper;

package body IOP.Helper is

   package IDL_Sequence_IOP_TaggedComponent_Helper is
     new IDL_Sequence_IOP_TaggedComponent.CORBA_Helper
       (Element_To_Any   => To_Any,
        Element_From_Any => From_Any);

   function From_Any (Item : in CORBA.Any)
      return IDL_Sequence_IOP_TaggedComponent.Sequence
      renames IDL_Sequence_IOP_TaggedComponent_Helper.From_Any;

   function To_Any
     (Item : in IDL_Sequence_IOP_TaggedComponent.Sequence)
      return CORBA.Any
      renames IDL_Sequence_IOP_TaggedComponent_Helper.To_Any;

   package IDL_Sequence_IOP_TaggedProfile_Helper is
     new IDL_Sequence_IOP_TaggedProfile.CORBA_Helper
       (Element_To_Any   => To_Any,
        Element_From_Any => From_Any);

   function From_Any (Item : in CORBA.Any)
      return IDL_Sequence_IOP_TaggedProfile.Sequence
      renames IDL_Sequence_IOP_TaggedProfile_Helper.From_Any;

   function To_Any
     (Item : in IDL_Sequence_IOP_TaggedProfile.Sequence)
      return CORBA.Any
      renames IDL_Sequence_IOP_TaggedProfile_Helper.To_Any;

   package IDL_Sequence_Octet_Helper is
     new IDL_Sequence_Octet.CORBA_Helper
       (Element_To_Any   => CORBA.To_Any,
        Element_From_Any => CORBA.From_Any);

   function From_Any (Item : in CORBA.Any)
      return IDL_Sequence_Octet.Sequence
      renames IDL_Sequence_Octet_Helper.From_Any;

   function To_Any
     (Item : in IDL_Sequence_Octet.Sequence)
      return CORBA.Any
      renames IDL_Sequence_Octet_Helper.To_Any;

   package IDL_Sequence_IOP_ServiceContext_Helper is
     new IDL_Sequence_IOP_ServiceContext.CORBA_Helper
       (Element_To_Any   => To_Any,
        Element_From_Any => From_Any);

   function From_Any (Item : in CORBA.Any)
      return IDL_Sequence_IOP_ServiceContext.Sequence
      renames IDL_Sequence_IOP_ServiceContext_Helper.From_Any;

   function To_Any
     (Item : in IDL_Sequence_IOP_ServiceContext.Sequence)
      return CORBA.Any
      renames IDL_Sequence_IOP_ServiceContext_Helper.To_Any;

   --------------
   -- From_Any --
   --------------

   function From_Any (Item : in CORBA.Any) return ComponentId is
      Result : constant CORBA.Unsigned_Long := CORBA.From_Any (Item);
   begin
      return ComponentId (Result);
   end From_Any;

   function From_Any (Item : in CORBA.Any) return Encoding is
      Index                : CORBA.Any;
      Result_Format        : EncodingFormat;
      Result_Major_Version : CORBA.Octet;
      Result_Minor_Version : CORBA.Octet;
   begin
      Index :=
        CORBA.Get_Aggregate_Element
        (Item, TC_EncodingFormat, CORBA.Unsigned_Long (0));
      Result_Format := From_Any (Index);

      Index :=
        CORBA.Get_Aggregate_Element
        (Item, CORBA.TC_Octet, CORBA.Unsigned_Long (1));
      Result_Major_Version := CORBA.From_Any (Index);

      Index :=
        CORBA.Get_Aggregate_Element
        (Item, CORBA.TC_Octet, CORBA.Unsigned_Long (2));
      Result_Minor_Version := CORBA.From_Any (Index);

      return
        (Format        => Result_Format,
         Major_Version => Result_Major_Version,
         Minor_Version => Result_Minor_Version);
   end From_Any;

   function From_Any (Item : in CORBA.Any) return EncodingFormat is
      Result : constant CORBA.Short := CORBA.From_Any (Item);
   begin
      return EncodingFormat (Result);
   end From_Any;

   function From_Any (Item : in CORBA.Any) return IOR is
      Index           : CORBA.Any;
      Result_Type_Id  : CORBA.String;
      Result_Profiles : IDL_Sequence_IOP_TaggedProfile.Sequence;
   begin
      Index :=
        CORBA.Get_Aggregate_Element
        (Item, CORBA.TC_String, CORBA.Unsigned_Long (0));
      Result_Type_Id := CORBA.From_Any (Index);

      Index :=
        CORBA.Get_Aggregate_Element
        (Item, TC_IDL_Sequence_IOP_TaggedProfile, CORBA.Unsigned_Long (1));
      Result_Profiles := From_Any (Index);

      return
        (Type_Id  => Result_Type_Id,
         Profiles => Result_Profiles);
   end From_Any;

   function From_Any (Item : in CORBA.Any) return MultipleComponentProfile is
      Result : constant IDL_Sequence_IOP_TaggedComponent.Sequence
        := From_Any (Item);
   begin
      return MultipleComponentProfile (Result);
   end From_Any;

   function From_Any (Item : in CORBA.Any) return ProfileId is
      Result : constant CORBA.Unsigned_Long := CORBA.From_Any (Item);
   begin
      return ProfileId (Result);
   end From_Any;

   function From_Any (Item : in CORBA.Any) return ServiceContext is
      Index               : CORBA.Any;
      Result_Context_Id   : ServiceId;
      Result_Context_Data : IDL_Sequence_Octet.Sequence;
   begin
      Index :=
        CORBA.Get_Aggregate_Element
        (Item, TC_ServiceId, CORBA.Unsigned_Long (0));
      Result_Context_Id := From_Any (Index);

      Index :=
        CORBA.Get_Aggregate_Element
        (Item, TC_IDL_Sequence_Octet, CORBA.Unsigned_Long (1));
      Result_Context_Data := From_Any (Index);

      return
        (Context_Id   => Result_Context_Id,
         Context_Data => Result_Context_Data);
   end From_Any;

   function From_Any (Item : in CORBA.Any) return ServiceContextList is
      Result : constant IDL_Sequence_IOP_ServiceContext.Sequence
        := From_Any (Item);
   begin
      return ServiceContextList (Result);
   end From_Any;

   function From_Any (Item : in CORBA.Any) return ServiceId is
      Result : constant CORBA.Unsigned_Long := CORBA.From_Any (Item);
   begin
      return ServiceId (Result);
   end From_Any;

   function From_Any (Item : in CORBA.Any) return TaggedComponent is
      Index                 : CORBA.Any;
      Result_Tag            : ComponentId;
      Result_Component_Data : IDL_Sequence_Octet.Sequence;
   begin
      Index :=
        CORBA.Get_Aggregate_Element
        (Item, TC_ComponentId, CORBA.Unsigned_Long (0));
      Result_Tag := From_Any (Index);

      Index :=
        CORBA.Get_Aggregate_Element
        (Item, TC_IDL_Sequence_Octet, CORBA.Unsigned_Long (1));
      Result_Component_Data := From_Any (Index);

      return
        (Tag            => Result_Tag,
         Component_Data => Result_Component_Data);
   end From_Any;

   function From_Any (Item : in CORBA.Any) return TaggedComponentSeq is
      Result : constant IDL_Sequence_IOP_TaggedComponent.Sequence
        := From_Any (Item);
   begin
      return TaggedComponentSeq (Result);
   end From_Any;

   function From_Any (Item : in CORBA.Any) return TaggedProfile is
      Index               : CORBA.Any;
      Result_Tag          : ProfileId;
      Result_Profile_Data : IDL_Sequence_Octet.Sequence;
   begin
      Index :=
        CORBA.Get_Aggregate_Element
        (Item, TC_ProfileId, CORBA.Unsigned_Long (0));
      Result_Tag := From_Any (Index);

      Index :=
        CORBA.Get_Aggregate_Element
        (Item, TC_IDL_Sequence_Octet, CORBA.Unsigned_Long (1));
      Result_Profile_Data := From_Any (Index);

      return
        (Tag          => Result_Tag,
         Profile_Data => Result_Profile_Data);
   end From_Any;

   ------------
   -- To_Any --
   ------------

   function To_Any (Item : in ComponentId) return CORBA.Any is
      Result : CORBA.Any := CORBA.To_Any (CORBA.Unsigned_Long (Item));
   begin
      CORBA.Set_Type (Result, TC_ComponentId);
      return Result;
   end To_Any;

   function To_Any (Item : in Encoding) return CORBA.Any is
      Result : CORBA.Any := CORBA.Get_Empty_Any_Aggregate (TC_Encoding);
   begin
      CORBA.Add_Aggregate_Element (Result, To_Any (Item.Format));
      CORBA.Add_Aggregate_Element (Result, CORBA.To_Any (Item.Major_Version));
      CORBA.Add_Aggregate_Element (Result, CORBA.To_Any (Item.Minor_Version));
      return Result;
   end To_Any;

   function To_Any (Item : in EncodingFormat) return CORBA.Any is
      Result : CORBA.Any := CORBA.To_Any (CORBA.Short (Item));
   begin
      CORBA.Set_Type (Result, TC_EncodingFormat);
      return Result;
   end To_Any;

   function To_Any (Item : in IOR) return CORBA.Any is
      Result : CORBA.Any := CORBA.Get_Empty_Any_Aggregate (TC_IOR);
   begin
      CORBA.Add_Aggregate_Element (Result, CORBA.To_Any (Item.Type_Id));
      CORBA.Add_Aggregate_Element (Result, To_Any (Item.Profiles));
      return Result;
   end To_Any;

   function To_Any (Item : in MultipleComponentProfile) return CORBA.Any is
      Result : CORBA.Any
        := To_Any (IDL_Sequence_IOP_TaggedComponent.Sequence (Item));
   begin
      CORBA.Set_Type (Result, TC_MultipleComponentProfile);
      return Result;
   end To_Any;

   function To_Any (Item : in ProfileId) return CORBA.Any is
      Result : CORBA.Any := CORBA.To_Any (CORBA.Unsigned_Long (Item));
   begin
      CORBA.Set_Type (Result, TC_ProfileId);
      return Result;
   end To_Any;

   function To_Any (Item : in ServiceContext) return CORBA.Any is
      Result : CORBA.Any := CORBA.Get_Empty_Any_Aggregate (TC_ServiceContext);
   begin
      CORBA.Add_Aggregate_Element (Result, To_Any (Item.Context_Id));
      CORBA.Add_Aggregate_Element (Result, To_Any (Item.Context_Data));
      return Result;
   end To_Any;

   function To_Any (Item : in ServiceContextList) return CORBA.Any is
      Result : CORBA.Any
        := To_Any (IDL_Sequence_IOP_ServiceContext.Sequence (Item));
   begin
      CORBA.Set_Type (Result, TC_ServiceContextList);
      return Result;
   end To_Any;

   function To_Any (Item : in ServiceId) return CORBA.Any is
      Result : CORBA.Any := CORBA.To_Any (CORBA.Unsigned_Long (Item));
   begin
      CORBA.Set_Type (Result, TC_ServiceId);
      return Result;
   end To_Any;

   function To_Any (Item : in TaggedComponent) return CORBA.Any is
      Result : CORBA.Any := CORBA.Get_Empty_Any_Aggregate (TC_TaggedComponent);
   begin
      CORBA.Add_Aggregate_Element (Result, To_Any (Item.Tag));
      CORBA.Add_Aggregate_Element (Result, To_Any (Item.Component_Data));
      return Result;
   end To_Any;

   function To_Any (Item : in TaggedComponentSeq) return CORBA.Any is
      Result : CORBA.Any
        := To_Any (IDL_Sequence_IOP_TaggedComponent.Sequence (Item));
   begin
      CORBA.Set_Type (Result, TC_TaggedComponentSeq);
      return Result;
   end To_Any;

   function To_Any (Item : in TaggedProfile) return CORBA.Any is
      Result : CORBA.Any := CORBA.Get_Empty_Any_Aggregate (TC_TaggedProfile);
   begin
      CORBA.Add_Aggregate_Element (Result, To_Any (Item.Tag));
      CORBA.Add_Aggregate_Element (Result, To_Any (Item.Profile_Data));

      return Result;
   end To_Any;

   -----------------------------
   -- Deferred_Initialization --
   -----------------------------

   procedure Deferred_Initialization;

   procedure Deferred_Initialization is
   begin
      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("ProfileId");
         Id   : CORBA.String
           := CORBA.To_CORBA_String (ProfileId_Repository_Id);
      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_ProfileId, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_ProfileId, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_ProfileId, CORBA.To_Any (CORBA.TC_Unsigned_Long));
      end;

      TC_IDL_Sequence_Octet :=
        CORBA.TypeCode.Internals.Build_Sequence_TC (CORBA.TC_Octet, 0);
      IDL_Sequence_Octet_Helper.Initialize
       (Element_TC  => CORBA.TC_Octet,
        Sequence_TC => TC_IDL_Sequence_Octet);

      declare
         Name                  : CORBA.String
           := CORBA.To_CORBA_String ("TaggedProfile");
         Id                    : CORBA.String
           := CORBA.To_CORBA_String (TaggedProfile_Repository_Id);
         Arg_Name_Tag          : CORBA.String
           := CORBA.To_CORBA_String ("tag");
         Arg_Name_Profile_Data : CORBA.String
           := CORBA.To_CORBA_String ("profile_data");
      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_TaggedProfile, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_TaggedProfile, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_TaggedProfile, CORBA.To_Any (TC_ProfileId));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_TaggedProfile, CORBA.To_Any (Arg_Name_Tag));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_TaggedProfile, CORBA.To_Any (TC_IDL_Sequence_Octet));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_TaggedProfile, CORBA.To_Any (Arg_Name_Profile_Data));
      end;

      TC_IDL_Sequence_IOP_TaggedProfile :=
        CORBA.TypeCode.Internals.Build_Sequence_TC (TC_TaggedProfile, 0);
      IDL_Sequence_IOP_TaggedProfile_Helper.Initialize
       (Element_TC  => TC_TaggedProfile,
        Sequence_TC => TC_IDL_Sequence_IOP_TaggedProfile);

      declare
         Name              : CORBA.String := CORBA.To_CORBA_String ("IOR");
         Id                : CORBA.String
           := CORBA.To_CORBA_String (IOR_Repository_Id);
         Arg_Name_Type_Id  : CORBA.String := CORBA.To_CORBA_String ("type_id");
         Arg_Name_Profiles : CORBA.String
           := CORBA.To_CORBA_String ("profiles");

      begin
         CORBA.TypeCode.Internals.Add_Parameter (TC_IOR, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter (TC_IOR, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_IOR, CORBA.To_Any (CORBA.TC_String));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_IOR, CORBA.To_Any (Arg_Name_Type_Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_IOR, CORBA.To_Any (TC_IDL_Sequence_IOP_TaggedProfile));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_IOR, CORBA.To_Any (Arg_Name_Profiles));
      end;

      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("ComponentId");
         Id   : CORBA.String
           := CORBA.To_CORBA_String (ComponentId_Repository_Id);
      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_ComponentId, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_ComponentId, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_ComponentId, CORBA.To_Any (CORBA.TC_Unsigned_Long));
      end;

      declare
         Name                    : CORBA.String
           := CORBA.To_CORBA_String ("TaggedComponent");
         Id                      : CORBA.String
           := CORBA.To_CORBA_String (TaggedComponent_Repository_Id);
         Arg_Name_Tag            : CORBA.String
           := CORBA.To_CORBA_String ("tag");
         Arg_Name_Component_Data : CORBA.String
           := CORBA.To_CORBA_String ("component_data");

      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_TaggedComponent, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_TaggedComponent, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_TaggedComponent, CORBA.To_Any (TC_ComponentId));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_TaggedComponent, CORBA.To_Any (Arg_Name_Tag));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_TaggedComponent, CORBA.To_Any (TC_IDL_Sequence_Octet));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_TaggedComponent, CORBA.To_Any (Arg_Name_Component_Data));
      end;

      TC_IDL_Sequence_IOP_TaggedComponent :=
        CORBA.TypeCode.Internals.Build_Sequence_TC (TC_TaggedComponent, 0);
      IDL_Sequence_IOP_TaggedComponent_Helper.Initialize
        (Element_TC  => TC_TaggedComponent,
         Sequence_TC => TC_IDL_Sequence_IOP_TaggedComponent);

      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("TaggedComponentSeq");
         Id   : CORBA.String
           := CORBA.To_CORBA_String (TaggedComponentSeq_Repository_Id);
      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_TaggedComponentSeq, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_TaggedComponentSeq, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_TaggedComponentSeq,
            CORBA.To_Any (TC_IDL_Sequence_IOP_TaggedComponent));
      end;

      declare
         Name : CORBA.String
           := CORBA.To_CORBA_String ("MultipleComponentProfile");
         Id   : CORBA.String
           := CORBA.To_CORBA_String (MultipleComponentProfile_Repository_Id);
      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_MultipleComponentProfile, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_MultipleComponentProfile, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_MultipleComponentProfile,
            CORBA.To_Any (TC_IDL_Sequence_IOP_TaggedComponent));
      end;

      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("ServiceId");
         Id   : CORBA.String
           := CORBA.To_CORBA_String (ServiceId_Repository_Id);
      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_ServiceId, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_ServiceId, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_ServiceId, CORBA.To_Any (CORBA.TC_Unsigned_Long));
      end;

      declare
         Name                  : CORBA.String
           := CORBA.To_CORBA_String ("ServiceContext");
         Id                    : CORBA.String
           := CORBA.To_CORBA_String (ServiceContext_Repository_Id);
         Arg_Name_Context_Id   : CORBA.String
           := CORBA.To_CORBA_String ("context_id");
         Arg_Name_Context_Data : CORBA.String
           := CORBA.To_CORBA_String ("context_data");
      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_ServiceContext, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_ServiceContext, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_ServiceContext, CORBA.To_Any (TC_ServiceId));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_ServiceContext, CORBA.To_Any (Arg_Name_Context_Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_ServiceContext, CORBA.To_Any (TC_IDL_Sequence_Octet));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_ServiceContext, CORBA.To_Any (Arg_Name_Context_Data));
      end;

      TC_IDL_Sequence_IOP_ServiceContext :=
        CORBA.TypeCode.Internals.Build_Sequence_TC (TC_ServiceContext, 0);
      IDL_Sequence_IOP_ServiceContext_Helper.Initialize
       (Element_TC  => TC_ServiceContext,
        Sequence_TC => TC_IDL_Sequence_IOP_ServiceContext);

      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("ServiceContextList");
         Id   : CORBA.String
           := CORBA.To_CORBA_String (ServiceContextList_Repository_Id);
      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_ServiceContextList, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_ServiceContextList, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_ServiceContextList,
            CORBA.To_Any (TC_IDL_Sequence_IOP_ServiceContext));
      end;

      declare
         Name : CORBA.String := CORBA.To_CORBA_String ("EncodingFormat");
         Id   : CORBA.String
           := CORBA.To_CORBA_String (EncodingFormat_Repository_Id);
      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_EncodingFormat, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_EncodingFormat, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_EncodingFormat, CORBA.To_Any (CORBA.TC_Short));
      end;

      declare
         Name                   : CORBA.String
           := CORBA.To_CORBA_String ("Encoding");
         Id                     : CORBA.String
           := CORBA.To_CORBA_String (Encoding_Repository_Id);
         Arg_Name_Format        : CORBA.String
           := CORBA.To_CORBA_String ("format");
         Arg_Name_Major_Version : CORBA.String
           := CORBA.To_CORBA_String ("major_version");
         Arg_Name_Minor_Version : CORBA.String
           := CORBA.To_CORBA_String ("minor_version");
      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_Encoding, CORBA.To_Any (Name));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_Encoding, CORBA.To_Any (Id));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_Encoding, CORBA.To_Any (TC_EncodingFormat));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_Encoding, CORBA.To_Any (Arg_Name_Format));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_Encoding, CORBA.To_Any (CORBA.TC_Octet));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_Encoding, CORBA.To_Any (Arg_Name_Major_Version));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_Encoding, CORBA.To_Any (CORBA.TC_Octet));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_Encoding, CORBA.To_Any (Arg_Name_Minor_Version));
      end;
   end Deferred_Initialization;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;
   begin
      Register_Module
        (Module_Info'
         (Name      => +"IOP.Helper",
          Conflicts => Empty,
          Depends   => Empty,
          Provides  => Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access));
   end;
end IOP.Helper;
