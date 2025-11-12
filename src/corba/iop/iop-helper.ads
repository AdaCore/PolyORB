pragma Style_Checks ("NM32766");
pragma Wide_Character_Encoding (Brackets);
pragma Warnings (Off, "use of an anonymous access type allocator");

---------------------------------------------------
--  This file has been generated automatically from
--  /Users/heathdorn/Documents/Playground/Agents/RefactorTeam/code_refactor/PolyORB/idls/Interop/IOP.idl
--  by IAC (IDL to Ada Compiler) 20.0w (rev. 41a9b833).
---------------------------------------------------
--  NOTE: If you modify this file by hand, your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
---------------------------------------------------

with CORBA;
pragma Elaborate_All (CORBA);
with PolyORB.Any;
with PolyORB.Types;
with PolyORB.Sequences.Unbounded.CORBA_Helper;
pragma Elaborate_All (PolyORB.Sequences.Unbounded.CORBA_Helper);

package IOP.Helper is

   TC_ProfileId : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return IOP.ProfileId;

   function To_Any
     (Item : IOP.ProfileId)
     return CORBA.Any;

   TC_ProfileData : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return IOP.ProfileData;

   function To_Any
     (Item : IOP.ProfileData)
     return CORBA.Any;

   TC_TaggedProfile : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return IOP.TaggedProfile;

   function To_Any
     (Item : IOP.TaggedProfile)
     return CORBA.Any;

   TC_IDL_SEQUENCE_IOP_TaggedProfile : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return IOP.IDL_SEQUENCE_IOP_TaggedProfile.Sequence;

   function To_Any
     (Item : IOP.IDL_SEQUENCE_IOP_TaggedProfile.Sequence)
     return CORBA.Any;

   TC_TaggedProfileSeq : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return IOP.TaggedProfileSeq;

   function To_Any
     (Item : IOP.TaggedProfileSeq)
     return CORBA.Any;

   TC_IOR : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return IOP.IOR;

   function To_Any
     (Item : IOP.IOR)
     return CORBA.Any;

   TC_ComponentId : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return IOP.ComponentId;

   function To_Any
     (Item : IOP.ComponentId)
     return CORBA.Any;

   TC_ComponentData : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return IOP.ComponentData;

   function To_Any
     (Item : IOP.ComponentData)
     return CORBA.Any;

   TC_TaggedComponent : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return IOP.TaggedComponent;

   function To_Any
     (Item : IOP.TaggedComponent)
     return CORBA.Any;

   TC_IDL_SEQUENCE_IOP_TaggedComponent : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return IOP.IDL_SEQUENCE_IOP_TaggedComponent.Sequence;

   function To_Any
     (Item : IOP.IDL_SEQUENCE_IOP_TaggedComponent.Sequence)
     return CORBA.Any;

   TC_TaggedComponentSeq : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return IOP.TaggedComponentSeq;

   function To_Any
     (Item : IOP.TaggedComponentSeq)
     return CORBA.Any;

   TC_ObjectKey : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return IOP.ObjectKey;

   function To_Any
     (Item : IOP.ObjectKey)
     return CORBA.Any;

   TC_IDL_SEQUENCE_IOP_TaggedComponent_1 : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return IOP.IDL_SEQUENCE_IOP_TaggedComponent_1.Sequence;

   function To_Any
     (Item : IOP.IDL_SEQUENCE_IOP_TaggedComponent_1.Sequence)
     return CORBA.Any;

   TC_MultipleComponentProfile : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return IOP.MultipleComponentProfile;

   function To_Any
     (Item : IOP.MultipleComponentProfile)
     return CORBA.Any;

   TC_ContextData : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return IOP.ContextData;

   function To_Any
     (Item : IOP.ContextData)
     return CORBA.Any;

   TC_ServiceId : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return IOP.ServiceId;

   function To_Any
     (Item : IOP.ServiceId)
     return CORBA.Any;

   TC_ServiceContext : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return IOP.ServiceContext;

   function To_Any
     (Item : IOP.ServiceContext)
     return CORBA.Any;

   TC_IDL_SEQUENCE_IOP_ServiceContext : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return IOP.IDL_SEQUENCE_IOP_ServiceContext.Sequence;

   function To_Any
     (Item : IOP.IDL_SEQUENCE_IOP_ServiceContext.Sequence)
     return CORBA.Any;

   TC_ServiceContextList : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return IOP.ServiceContextList;

   function To_Any
     (Item : IOP.ServiceContextList)
     return CORBA.Any;

   TC_EncodingFormat : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return IOP.EncodingFormat;

   function To_Any
     (Item : IOP.EncodingFormat)
     return CORBA.Any;

   TC_Encoding : CORBA.TypeCode.Object;

   function From_Any
     (Item : CORBA.Any)
     return IOP.Encoding;

   function To_Any
     (Item : IOP.Encoding)
     return CORBA.Any;

   
   package Internals is

      function Wrap
        (X : access IOP.ProfileId)
        return PolyORB.Any.Content'Class;

      procedure Initialize_ProfileId;

      procedure Initialize_ProfileData;

      type Ptr_Ü_TaggedProfile is
        access all IOP.TaggedProfile;

      type Content_Ü_TaggedProfile is
        new PolyORB.Any.Aggregate_Content with record
            V : Ptr_Ü_TaggedProfile;
         end record;

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_TaggedProfile;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

      function Get_Aggregate_Count
        (Acc : Content_Ü_TaggedProfile)
        return PolyORB.Types.Unsigned_Long;

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_TaggedProfile;
         Count : PolyORB.Types.Unsigned_Long);

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_TaggedProfile)
        return PolyORB.Types.Address;

      function Clone
        (Acc : Content_Ü_TaggedProfile;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;

      procedure Finalize_Value
        (Acc : in out Content_Ü_TaggedProfile);

      function Wrap
        (X : access IOP.TaggedProfile)
        return PolyORB.Any.Content'Class;

      procedure Initialize_TaggedProfile;

      function IDL_SEQUENCE_IOP_TaggedProfile_Element_Wrap
        (X : access IOP.TaggedProfile)
        return PolyORB.Any.Content'Class;

      function Wrap
        (X : access IOP.IDL_SEQUENCE_IOP_TaggedProfile.Sequence)
        return PolyORB.Any.Content'Class;

      package IDL_SEQUENCE_IOP_TaggedProfile_Helper is
        new IOP.IDL_SEQUENCE_IOP_TaggedProfile.CORBA_Helper
           (Element_From_Any => IOP.Helper.From_Any,
            Element_To_Any => IOP.Helper.To_Any,
            Element_Wrap => IOP.Helper.Internals.IDL_SEQUENCE_IOP_TaggedProfile_Element_Wrap);

      procedure Initialize_IDL_SEQUENCE_IOP_TaggedProfile;

      procedure Initialize_TaggedProfileSeq;

      type Ptr_Ü_IOR is
        access all IOP.IOR;

      type Content_Ü_IOR is
        new PolyORB.Any.Aggregate_Content with record
            V : Ptr_Ü_IOR;
         end record;

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_IOR;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

      function Get_Aggregate_Count
        (Acc : Content_Ü_IOR)
        return PolyORB.Types.Unsigned_Long;

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_IOR;
         Count : PolyORB.Types.Unsigned_Long);

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_IOR)
        return PolyORB.Types.Address;

      function Clone
        (Acc : Content_Ü_IOR;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;

      procedure Finalize_Value
        (Acc : in out Content_Ü_IOR);

      function Wrap
        (X : access IOP.IOR)
        return PolyORB.Any.Content'Class;

      procedure Initialize_IOR;

      function Wrap
        (X : access IOP.ComponentId)
        return PolyORB.Any.Content'Class;

      procedure Initialize_ComponentId;

      procedure Initialize_ComponentData;

      type Ptr_Ü_TaggedComponent is
        access all IOP.TaggedComponent;

      type Content_Ü_TaggedComponent is
        new PolyORB.Any.Aggregate_Content with record
            V : Ptr_Ü_TaggedComponent;
         end record;

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_TaggedComponent;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

      function Get_Aggregate_Count
        (Acc : Content_Ü_TaggedComponent)
        return PolyORB.Types.Unsigned_Long;

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_TaggedComponent;
         Count : PolyORB.Types.Unsigned_Long);

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_TaggedComponent)
        return PolyORB.Types.Address;

      function Clone
        (Acc : Content_Ü_TaggedComponent;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;

      procedure Finalize_Value
        (Acc : in out Content_Ü_TaggedComponent);

      function Wrap
        (X : access IOP.TaggedComponent)
        return PolyORB.Any.Content'Class;

      procedure Initialize_TaggedComponent;

      function IDL_SEQUENCE_IOP_TaggedComponent_Element_Wrap
        (X : access IOP.TaggedComponent)
        return PolyORB.Any.Content'Class;

      function Wrap
        (X : access IOP.IDL_SEQUENCE_IOP_TaggedComponent.Sequence)
        return PolyORB.Any.Content'Class;

      package IDL_SEQUENCE_IOP_TaggedComponent_Helper is
        new IOP.IDL_SEQUENCE_IOP_TaggedComponent.CORBA_Helper
           (Element_From_Any => IOP.Helper.From_Any,
            Element_To_Any => IOP.Helper.To_Any,
            Element_Wrap => IOP.Helper.Internals.IDL_SEQUENCE_IOP_TaggedComponent_Element_Wrap);

      procedure Initialize_IDL_SEQUENCE_IOP_TaggedComponent;

      procedure Initialize_TaggedComponentSeq;

      procedure Initialize_ObjectKey;

      function IDL_SEQUENCE_IOP_TaggedComponent_1_Element_Wrap
        (X : access IOP.TaggedComponent)
        return PolyORB.Any.Content'Class;

      function Wrap
        (X : access IOP.IDL_SEQUENCE_IOP_TaggedComponent_1.Sequence)
        return PolyORB.Any.Content'Class;

      package IDL_SEQUENCE_IOP_TaggedComponent_1_Helper is
        new IOP.IDL_SEQUENCE_IOP_TaggedComponent_1.CORBA_Helper
           (Element_From_Any => IOP.Helper.From_Any,
            Element_To_Any => IOP.Helper.To_Any,
            Element_Wrap => IOP.Helper.Internals.IDL_SEQUENCE_IOP_TaggedComponent_1_Element_Wrap);

      procedure Initialize_IDL_SEQUENCE_IOP_TaggedComponent_1;

      procedure Initialize_MultipleComponentProfile;

      procedure Initialize_ContextData;

      function Wrap
        (X : access IOP.ServiceId)
        return PolyORB.Any.Content'Class;

      procedure Initialize_ServiceId;

      type Ptr_Ü_ServiceContext is
        access all IOP.ServiceContext;

      type Content_Ü_ServiceContext is
        new PolyORB.Any.Aggregate_Content with record
            V : Ptr_Ü_ServiceContext;
         end record;

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_ServiceContext;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

      function Get_Aggregate_Count
        (Acc : Content_Ü_ServiceContext)
        return PolyORB.Types.Unsigned_Long;

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_ServiceContext;
         Count : PolyORB.Types.Unsigned_Long);

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_ServiceContext)
        return PolyORB.Types.Address;

      function Clone
        (Acc : Content_Ü_ServiceContext;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;

      procedure Finalize_Value
        (Acc : in out Content_Ü_ServiceContext);

      function Wrap
        (X : access IOP.ServiceContext)
        return PolyORB.Any.Content'Class;

      procedure Initialize_ServiceContext;

      function IDL_SEQUENCE_IOP_ServiceContext_Element_Wrap
        (X : access IOP.ServiceContext)
        return PolyORB.Any.Content'Class;

      function Wrap
        (X : access IOP.IDL_SEQUENCE_IOP_ServiceContext.Sequence)
        return PolyORB.Any.Content'Class;

      package IDL_SEQUENCE_IOP_ServiceContext_Helper is
        new IOP.IDL_SEQUENCE_IOP_ServiceContext.CORBA_Helper
           (Element_From_Any => IOP.Helper.From_Any,
            Element_To_Any => IOP.Helper.To_Any,
            Element_Wrap => IOP.Helper.Internals.IDL_SEQUENCE_IOP_ServiceContext_Element_Wrap);

      procedure Initialize_IDL_SEQUENCE_IOP_ServiceContext;

      procedure Initialize_ServiceContextList;

      function Wrap
        (X : access IOP.EncodingFormat)
        return PolyORB.Any.Content'Class;

      procedure Initialize_EncodingFormat;

      type Ptr_Ü_Encoding is
        access all IOP.Encoding;

      type Content_Ü_Encoding is
        new PolyORB.Any.Aggregate_Content with record
            V : Ptr_Ü_Encoding;
         end record;

      function Get_Aggregate_Element
        (Acc : not null access Content_Ü_Encoding;
         Tc : PolyORB.Any.TypeCode.Object_Ptr;
         Index : PolyORB.Types.Unsigned_Long;
         Mech : not null access PolyORB.Any.Mechanism)
        return PolyORB.Any.Content'Class;

      function Get_Aggregate_Count
        (Acc : Content_Ü_Encoding)
        return PolyORB.Types.Unsigned_Long;

      procedure Set_Aggregate_Count
        (Acc : in out Content_Ü_Encoding;
         Count : PolyORB.Types.Unsigned_Long);

      function Unchecked_Get_V
        (Acc : not null access Content_Ü_Encoding)
        return PolyORB.Types.Address;

      function Clone
        (Acc : Content_Ü_Encoding;
         Into : PolyORB.Any.Content_Ptr := null)
        return PolyORB.Any.Content_Ptr;

      procedure Finalize_Value
        (Acc : in out Content_Ü_Encoding);

      function Wrap
        (X : access IOP.Encoding)
        return PolyORB.Any.Content'Class;

      procedure Initialize_Encoding;

   end Internals;

end IOP.Helper;
