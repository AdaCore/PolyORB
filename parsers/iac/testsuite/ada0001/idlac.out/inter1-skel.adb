-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.adacore.com/polyorb/)
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
-------------------------------------------------
pragma Style_Checks (Off);

with PolyORB.Utils.Strings;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization);
with PolyORB.CORBA_P.Domain_Management;
with PolyORB.CORBA_P.IR_Hooks;
with CORBA.Object.Helper;
with CORBA.ORB;
with CORBA.NVList;
with CORBA.ServerRequest;
with Inter1.Impl;
with CORBA;
pragma Elaborate_All (CORBA);
with PortableServer;
pragma Elaborate_All (PortableServer);
with PolyORB.CORBA_P.Exceptions;

package body Inter1.Skel is

   --  Skeleton subprograms

   function Servant_Is_A
     (Obj : PortableServer.Servant)
     return Boolean;

   function Servant_Is_A
     (Obj : PortableServer.Servant)
     return Boolean is
   begin
      return Obj.all in Inter1.Impl.Object'Class;
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
            Type_Id : CORBA.String;
            Arg_Name_Ü_Type_Id : constant CORBA.Identifier
            := CORBA.To_CORBA_String ("Type_Id");
            Argument_Ü_Type_Id : CORBA.Any := CORBA.To_Any (Type_Id);
            
            Result_Ü : CORBA.Boolean;
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
               Result_Ü := Inter1.Is_A
                 (CORBA.To_Standard_String (Type_Id));
            end;

            -- Set Result

            CORBA.ServerRequest.Set_Result
            (Request,
            CORBA.To_Any (Result_Ü));
            return;
         end;

      elsif Operation = "_interface" then

         CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

         CORBA.ServerRequest.Set_Result
           (Request,
            CORBA.Object.Helper.To_Any
            (CORBA.Object.Ref
             (PolyORB.CORBA_P.IR_Hooks.Get_Interface_Definition
              (CORBA.To_CORBA_String (Repository_Id)))));

         return;

      elsif Operation = "_domain_managers" then

         CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

         CORBA.ServerRequest.Set_Result
           (Request,
            PolyORB.CORBA_P.Domain_Management.Get_Domain_Managers
            (Self));

         return;

      elsif Operation = "_get_Attr1" then

         declare
            Result_Ü      : CORBA.Float;
         begin

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any


               --  Call implementation
               Result_Ü := Inter1.Impl.get_Attr1
                 (Inter1.Impl.Object'Class (Self.all)'Access);
            end;

            -- Set result

            CORBA.ServerRequest.Set_Result
              (Request, 
               CORBA.To_Any (Result_Ü));
            return;
         end;

      elsif Operation = "_set_Attr1" then

         declare
            To            : CORBA.Float;
            Arg_Name_Ü_To : constant CORBA.Identifier :=
              CORBA.To_CORBA_String ("To");
            Argument_Ü_To : CORBA.Any := CORBA.Internals.Get_Empty_Any
              (CORBA.TC_Float);

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
                 CORBA.From_Any (Argument_Ü_To);

               --  Call implementation
               Inter1.Impl.set_Attr1
                 (Inter1.Impl.Object'Class (Self.all)'Access,
                  To);
            end;
            return;
         end;

      elsif Operation = "_get_Attr2" then

         declare
            Result_Ü      : CORBA.Boolean;
         begin

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any


               --  Call implementation
               Result_Ü := Inter1.Impl.get_Attr2
                 (Inter1.Impl.Object'Class (Self.all)'Access);
            end;

            -- Set result

            CORBA.ServerRequest.Set_Result
              (Request, 
               CORBA.To_Any (Result_Ü));
            return;
         end;

      elsif Operation = "_set_Attr2" then

         declare
            To            : CORBA.Boolean;
            Arg_Name_Ü_To : constant CORBA.Identifier :=
              CORBA.To_CORBA_String ("To");
            Argument_Ü_To : CORBA.Any := CORBA.Internals.Get_Empty_Any
              (CORBA.TC_Boolean);

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
                 CORBA.From_Any (Argument_Ü_To);

               --  Call implementation
               Inter1.Impl.set_Attr2
                 (Inter1.Impl.Object'Class (Self.all)'Access,
                  To);
            end;
            return;
         end;

      elsif Operation = "_get_Attr3" then

         declare
            Result_Ü      : CORBA.Long;
         begin

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any


               --  Call implementation
               Result_Ü := Inter1.Impl.get_Attr3
                 (Inter1.Impl.Object'Class (Self.all)'Access);
            end;

            -- Set result

            CORBA.ServerRequest.Set_Result
              (Request, 
               CORBA.To_Any (Result_Ü));
            return;
         end;

      elsif Operation = "_get_Attr4" then

         declare
            Result_Ü      : CORBA.Long_Long;
         begin

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any


               --  Call implementation
               Result_Ü := Inter1.Impl.get_Attr4
                 (Inter1.Impl.Object'Class (Self.all)'Access);
            end;

            -- Set result

            CORBA.ServerRequest.Set_Result
              (Request, 
               CORBA.To_Any (Result_Ü));
            return;
         end;

      elsif Operation = "_set_Attr4" then

         declare
            To            : CORBA.Long_Long;
            Arg_Name_Ü_To : constant CORBA.Identifier :=
              CORBA.To_CORBA_String ("To");
            Argument_Ü_To : CORBA.Any := CORBA.Internals.Get_Empty_Any
              (CORBA.TC_Long_Long);

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
                 CORBA.From_Any (Argument_Ü_To);

               --  Call implementation
               Inter1.Impl.set_Attr4
                 (Inter1.Impl.Object'Class (Self.all)'Access,
                  To);
            end;
            return;
         end;

      elsif Operation = "Name" then

         declare
            code            : CORBA.Short;
            Arg_Name_Ü_code : constant CORBA.Identifier :=
              CORBA.To_CORBA_String ("code");
            Argument_Ü_code : CORBA.Any := CORBA.Internals.Get_Empty_Any
              (CORBA.TC_Short);

            Result_Ü        : CORBA.String;
         begin
            CORBA.NVList.Add_Item
              (Arg_List_Ü,
               Arg_Name_Ü_code,
               Argument_Ü_code,
               CORBA.ARG_IN);

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any

               code :=
                 CORBA.From_Any (Argument_Ü_code);

               --  Call implementation
               Result_Ü := Inter1.Impl.Name
                 (Inter1.Impl.Object'Class (Self.all)'Access,
                  code);
            end;

            -- Set result

            CORBA.ServerRequest.Set_Result
              (Request, 
               CORBA.To_Any (Result_Ü));
            return;
         end;

      elsif Operation = "SName" then

         declare
            code            : CORBA.Short;
            Arg_Name_Ü_code : constant CORBA.Identifier :=
              CORBA.To_CORBA_String ("code");
            Argument_Ü_code : CORBA.Any := CORBA.Internals.Get_Empty_Any
              (CORBA.TC_Short);

            str             : CORBA.String;
            Arg_Name_Ü_str  : constant CORBA.Identifier :=
              CORBA.To_CORBA_String ("str");
            Argument_Ü_str  : CORBA.Any := CORBA.Internals.Get_Empty_Any
              (CORBA.TC_String);

         begin
            CORBA.NVList.Add_Item
              (Arg_List_Ü,
               Arg_Name_Ü_code,
               Argument_Ü_code,
               CORBA.ARG_IN);
            CORBA.NVList.Add_Item
              (Arg_List_Ü,
               Arg_Name_Ü_str,
               Argument_Ü_str,
               CORBA.ARG_IN);

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any

               code :=
                 CORBA.From_Any (Argument_Ü_code);
               str :=
                 CORBA.From_Any (Argument_Ü_str);

               --  Call implementation
               Inter1.Impl.SName
                 (Inter1.Impl.Object'Class (Self.all)'Access,
                  code,
                  str);
            end;
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
      PortableServer.Internals.Register_Skeleton
        (CORBA.To_CORBA_String (Inter1.Repository_Id),
         Servant_Is_A'Access,
         Is_A'Access,
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
         (Name      => +"Inter1.Skel",
          Conflicts => Empty,
          Depends   =>
                  Empty
          ,
          Provides  => Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access));
   end;

end Inter1.Skel;
