-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.act-europe.fr/polyorb/)
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
-------------------------------------------------
pragma Style_Checks (Off);

with PolyORB.Utils.Strings;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization);
with EnumTests.Helper;
with CORBA.ORB;
with CORBA.NVList;
with CORBA.ServerRequest;
with EnumTests.Impl;
with CORBA;
pragma Elaborate_All (CORBA);
with PortableServer;
pragma Elaborate_All (PortableServer);
with PolyORB.CORBA_P.Exceptions;

package body EnumTests.Skel is

   --  Skeleton subprograms

   function Servant_Is_A
     (Obj : PortableServer.Servant)
     return Boolean;

   function Servant_Is_A
     (Obj : PortableServer.Servant)
     return Boolean is
   begin
      return Obj.all in EnumTests.Impl.Object'Class;
   end Servant_Is_A;

   procedure Invoke
     (Self : PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_ptr)
   is
      Operation : constant Standard.String
         := CORBA.To_Standard_String
              (CORBA.ServerRequest.Operation
               (Request.all));
      Arg_List_Ü : CORBA.NVList.Ref;
   begin
      CORBA.ORB.Create_List (0, Arg_List_Ü);
      if Operation = "_is_a" then
         declare
            Type_Id            : CORBA.String;
            Arg_Name_Ü_Type_Id : constant CORBA.Identifier
            := CORBA.To_CORBA_String ("Type_Id");
            Argument_Ü_Type_Id : CORBA.Any := CORBA.To_Any (Type_Id);
            
            Result_Ü           : CORBA.Boolean;
         begin
            CORBA.NVList.Add_Item
            (Arg_List_Ü,
            Arg_Name_Ü_Type_Id,
            Argument_Ü_Type_Id,
            CORBA.ARG_IN);

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any

               Type_Id :=
                 CORBA.From_Any (Argument_Ü_Type_Id);

               --  Call implementation
               Result_Ü := EnumTests.Is_A
                 (CORBA.To_Standard_String (Type_Id));
            end;

            -- Set Result

            CORBA.ServerRequest.Set_Result
            (Request,
            CORBA.To_Any (
            Result_Ü));
            return;
         end;

      elsif Operation = "_get_attr_enum" then

         declare
            Result_Ü      : EnumTests.Color;
         begin

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any


               --  Call implementation
               Result_Ü := EnumTests.Impl.get_attr_enum
                 (EnumTests.Impl.Object'Class (Self.all)'Access);
            end;

            -- Set Result

            CORBA.ServerRequest.Set_Result
              (Request, 
               EnumTests.Helper.To_Any (
               Result_Ü));
            return;
         end;

      elsif Operation = "_set_attr_enum" then

         declare
            To            : EnumTests.Color;
            Arg_Name_Ü_To : constant CORBA.Identifier
              := CORBA.To_CORBA_String ("To");
            Argument_Ü_To : CORBA.Any := CORBA.Get_Empty_Any
              (EnumTests.Helper.TC_Color);

         begin
            CORBA.NVList.Add_Item
              (Arg_List_Ü,
               Arg_Name_Ü_To,
               Argument_Ü_To,
               CORBA.ARG_IN);

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any

               To :=
                 EnumTests.Helper.From_Any (Argument_Ü_To);

               --  Call implementation
               EnumTests.Impl.set_attr_enum
                 (EnumTests.Impl.Object'Class (Self.all)'Access,
                  To);
            end;
            return;
         end;

      elsif Operation = "modif_enum" then

         declare
            C             : EnumTests.Color;
            Arg_Name_Ü_C  : constant CORBA.Identifier
              := CORBA.To_CORBA_String ("C");
            Argument_Ü_C  : CORBA.Any := CORBA.Get_Empty_Any
              (EnumTests.Helper.TC_Color);

            Returns       : EnumTests.Color;
         begin
            CORBA.NVList.Add_Item
              (Arg_List_Ü,
               Arg_Name_Ü_C,
               Argument_Ü_C,
               CORBA.ARG_INOUT);

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any

               C :=
                 EnumTests.Helper.From_Any (Argument_Ü_C);

               --  Call implementation
               EnumTests.Impl.modif_enum
                 (EnumTests.Impl.Object'Class (Self.all)'Access,
                  C,
                  Returns);
            end;

            --  Set out arguments.

            CORBA.Internals.Copy_Any_Value
              (Argument_Ü_C,
               EnumTests.Helper.To_Any
                 (C));


            -- Set Result

            CORBA.ServerRequest.Set_Result
              (Request, 
               EnumTests.Helper.To_Any (
               Returns));
            return;
         end;

      else
         CORBA.Raise_Bad_Operation (CORBA.Default_Sys_Member);
      end if;
   exception
      when E : others =>
         begin
            CORBA.ServerRequest.Set_Exception
              (Request,
               CORBA.Internals.To_CORBA_Any (PolyORB.CORBA_P.Exceptions.System_Exception_To_Any (E)));
            return;
         end;
   end Invoke;
   procedure Deferred_Initialization is
   begin
      null;
      PortableServer.Register_Skeleton
        (CORBA.To_CORBA_String (EnumTests.Repository_Id),
         Servant_Is_A'Access,
         Invoke'Access);
   end Deferred_Initialization;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;
   begin
      Register_Module
        (Module_Info'
         (Name      => +"EnumTests.Skel",
          Conflicts => Empty,
          Depends   =>
                  Empty
          ,
          Provides  => Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access));
   end;

end EnumTests.Skel;
