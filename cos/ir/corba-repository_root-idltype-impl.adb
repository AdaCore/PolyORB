pragma Style_Checks (Off);

with CORBA.Repository_Root; use CORBA.Repository_Root;
with CORBA.Repository_Root.IDLType.Skel;
pragma Warnings (Off, CORBA.Repository_Root.IDLType.Skel);
with CORBA.Repository_Root.IRObject.Impl;
with CORBA.Repository_Root.Interfacedef.Impl;
with CORBA.Repository_Root.Valuedef.Impl;
with CORBA.Repository_Root.UnionDef.Impl;
with CORBA.Repository_Root.StructDef.Impl;
with CORBA.Repository_Root.EnumDef.Impl;
with CORBA.Repository_Root.AliasDef.Impl;
with CORBA.Repository_Root.NativeDef.Impl;
with CORBA.Repository_Root.ValueBoxDef.Impl;
with CORBA.Repository_Root.TypedefDef.Impl;

package body CORBA.Repository_Root.IDLType.Impl is

   ------------------
   --  To_IDLType  --
   ------------------
   function To_IDLType
     (Self : IRObject.Impl.Object_Ptr)
     return  Object_ptr
   is
   begin
      case IRObject.Impl.Get_Def_Kind
        (Self) is
         when
           Dk_Repository |
           Dk_Attribute  |
           dk_ValueMember|
           Dk_Typedef    |
           Dk_Constant   |
           Dk_Operation  |
           Dk_Exception  |
           Dk_Module     |
           Dk_All        |
           Dk_None       =>
            CORBA.Raise_Internal (CORBA.Default_Sys_Member);
            return null;
         when
           --  inherited types
           Dk_Primitive  |
           Dk_String     |
           Dk_Sequence   |
           Dk_Array      |
           Dk_Wstring    |
           Dk_Fixed      =>
            return Object_Ptr (Self);
         -- types containing a "idltype_view" field
         when
           Dk_Alias      |
           Dk_Struct     |
           Dk_Union      |
           Dk_Enum       |
           Dk_ValueBox   |
           dk_Native =>
            declare
               Interm : constant TypedefDef.Impl.Object_Ptr :=
                 TypedefDef.Impl.Object_Ptr (Self);
            begin
               return TypedefDef.Impl.Get_IDLType_View (Interm);
            end;
         when
           Dk_Value      =>
            declare
               Interm : constant Valuedef.Impl.Object_Ptr :=
                 Valuedef.Impl.Object_Ptr (Self);
            begin
               return Valuedef.Impl.Get_IDLType_View (Interm);
            end;
         when
           Dk_Interface  =>
            declare
               Interm : constant Interfacedef.Impl.Object_Ptr :=
                 Interfacedef.Impl.Object_Ptr (Self);
            begin
               return Interfacedef.Impl.Get_IDLType_View (Interm);
            end;
      end case;
   end To_IDLType;


   ----------------------
   --  Procedure init  --
   ----------------------
--   procedure Init (Self : access Object;
--                   Real_Object : IRObject.Impl.Object_Ptr;
--                   Def_Kind : CORBA.Repository_Root.DefinitionKind) is
--   begin
--      pragma Debug (O2 ("init enter"));
--      IRObject.Impl.Init (IRObject.Impl.Object_Ptr (Self),
--                         Real_Object,
--                          Def_Kind);
--       pragma Debug (O2 ("init  end"));
--   end Init;


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
               Interm : constant Interfacedef.Impl.Object_Ptr :=
                 Interfacedef.Impl.Object_Ptr (Get_Real_Object (Self));
            begin
               return Interfacedef.Impl.Get_Type (Interm);
            end;
         when
           Dk_Value      =>
            declare
               Interm : constant Valuedef.Impl.Object_Ptr :=
                 Valuedef.Impl.Object_Ptr (Get_Real_Object (Self));
            begin
               return Valuedef.Impl.Get_Type (Interm);
            end;
         when  Dk_Struct     =>
            declare
               Interm : constant Structdef.Impl.Object_Ptr :=
                 Structdef.Impl.Object_Ptr (Get_Real_Object (Self));
            begin
               return Structdef.Impl.Get_Type (Interm);
            end;
         when  Dk_Union      =>
            declare
               Interm : constant Uniondef.Impl.Object_Ptr :=
                 Uniondef.Impl.Object_Ptr (Get_Real_Object (Self));
            begin
               return Uniondef.Impl.Get_Type (Interm);
            end;
         when  Dk_Enum      =>
            declare
               Interm : constant Enumdef.Impl.Object_Ptr :=
                 Enumdef.Impl.Object_Ptr (Get_Real_Object (Self));
            begin
               return Enumdef.Impl.Get_Type (Interm);
            end;
         when  Dk_Alias      =>
            declare
               Interm : constant Aliasdef.Impl.Object_Ptr :=
                 Aliasdef.Impl.Object_Ptr (Get_Real_Object (Self));
            begin
               return Aliasdef.Impl.Get_Type (Interm);
            end;
         when  Dk_Native      =>
            declare
               Interm : constant Nativedef.Impl.Object_Ptr :=
                 Nativedef.Impl.Object_Ptr (Get_Real_Object (Self));
            begin
               return Nativedef.Impl.Get_Type (Interm);
            end;
         when  Dk_ValueBox      =>
            declare
               Interm : constant ValueBoxdef.Impl.Object_Ptr :=
                 ValueBoxdef.Impl.Object_Ptr (Get_Real_Object (Self));
            begin
               return ValueBoxdef.Impl.Get_Type (Interm);
            end;
         when others =>
            CORBA.Raise_Internal (Default_Sys_Member);
            return CORBA.TC_Void;
      end case;

   end get_type;

end CORBA.Repository_Root.IDLType.Impl;
