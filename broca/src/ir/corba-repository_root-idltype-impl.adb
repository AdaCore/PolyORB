----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.Impl;

with Corba.Repository_Root; use Corba.Repository_Root;
with CORBA.Repository_Root.IDLType.Skel;
with CORBA.Repository_Root.IRObject.Impl;
with CORBA.Repository_Root.Interfacedef.Impl;
with CORBA.Repository_Root.Valuedef.Impl;
with CORBA.Repository_Root.UnionDef.Impl;
with CORBA.Repository_Root.StructDef.Impl;
with CORBA.Repository_Root.PrimitiveDef.Impl;
with CORBA.Repository_Root.StringDef.Impl;
with CORBA.Repository_Root.WstringDef.Impl;
with CORBA.Repository_Root.FixedDef.Impl;
with CORBA.Repository_Root.ArrayDef.Impl;
with CORBA.Repository_Root.SequenceDef.Impl;
with CORBA.Repository_Root.EnumDef.Impl;
with CORBA.Repository_Root.AliasDef.Impl;
with CORBA.Repository_Root.NativeDef.Impl;
with CORBA.Repository_Root.ValueBoxDef.Impl;

with Broca.Exceptions;

package body CORBA.Repository_Root.IDLType.Impl is

   -----------------
   --  To_Object  --
   -----------------
   function To_Object (Fw_Ref : IDLType_Forward.Ref)
                       return Object_Ptr is
   begin
      return Object_Ptr
        (IDLType.Object_Of
         (IDLType.Convert_Forward.To_Ref
          (Fw_Ref)));
   end To_Object;

   ------------------
   --  To_Forward  --
   ------------------
   function To_Forward (Obj : Object_Ptr)
                        return IDLType_Forward.Ref is
      Ref : IDLType.Ref;
   begin
      Set (Ref, CORBA.Impl.Object_Ptr (Obj));
      return IDLType.Convert_Forward.To_Forward (Ref);
   end To_Forward;

   ----------------------
   --  Procedure init  --
   ----------------------
   procedure Init (Self : access Object;
                   Real_Object : IRObject.Impl.Object_Ptr;
                   Def_Kind : CORBA.Repository_Root.DefinitionKind) is
   begin
      IRObject.Impl.Init (IRObject.Impl.Object_Ptr (Self),
                          Real_Object,
                          Def_Kind);
   end Init;


   function get_type
     (Self : access Object)
     return CORBA.TypeCode.Object
   is
   begin
      --  we are going to dispatch manually this call
      case Get_Def_Kind (Self) is
         when
           Dk_Primitive  |
           Dk_String     |
           Dk_Sequence   |
           Dk_Array      |
           Dk_Wstring    |
           Dk_Fixed      =>
            --  dispatching call
            return Get_Type (Object_Ptr (Self));
         when
           Dk_Interface  =>
            declare
               Interm : Interfacedef.Impl.Object_Ptr :=
                 Interfacedef.Impl.Object_Ptr (Get_Real_Object (Self));
            begin
               return Interfacedef.Impl.Get_Type (Interm);
            end;
         when
           Dk_Value      =>
            declare
               Interm : Valuedef.Impl.Object_Ptr :=
                 Valuedef.Impl.Object_Ptr (Get_Real_Object (Self));
            begin
               return Valuedef.Impl.Get_Type (Interm);
            end;
         when  Dk_Struct     =>
            declare
               Interm : Structdef.Impl.Object_Ptr :=
                 Structdef.Impl.Object_Ptr (Get_Real_Object (Self));
            begin
               return Structdef.Impl.Get_Type (Interm);
            end;
         when  Dk_Union      =>
            declare
               Interm : Uniondef.Impl.Object_Ptr :=
                 Uniondef.Impl.Object_Ptr (Get_Real_Object (Self));
            begin
               return Uniondef.Impl.Get_Type (Interm);
            end;
         when  Dk_Enum      =>
            declare
               Interm : Enumdef.Impl.Object_Ptr :=
                 Enumdef.Impl.Object_Ptr (Get_Real_Object (Self));
            begin
               return Enumdef.Impl.Get_Type (Interm);
            end;
         when  Dk_Alias      =>
            declare
               Interm : Aliasdef.Impl.Object_Ptr :=
                 Aliasdef.Impl.Object_Ptr (Get_Real_Object (Self));
            begin
               return Aliasdef.Impl.Get_Type (Interm);
            end;
         when  Dk_Native      =>
            declare
               Interm : Nativedef.Impl.Object_Ptr :=
                 Nativedef.Impl.Object_Ptr (Get_Real_Object (Self));
            begin
               return Nativedef.Impl.Get_Type (Interm);
            end;
         when  Dk_ValueBox      =>
            declare
               Interm : ValueBoxdef.Impl.Object_Ptr :=
                 ValueBoxdef.Impl.Object_Ptr (Get_Real_Object (Self));
            begin
               return ValueBoxdef.Impl.Get_Type (Interm);
            end;
         when others =>
            Broca.Exceptions.Raise_Internal;
            return CORBA.TC_Void;
      end case;

   end Get_Type;


end CORBA.Repository_Root.IDLType.Impl;
