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
with m1.Helper;
with m1.i1.Helper;
with PolyORB.CORBA_P.Domain_Management;
with PolyORB.CORBA_P.IR_Hooks;
with CORBA.Object.Helper;
with CORBA.ORB;
with CORBA.NVList;
with CORBA.ServerRequest;
with m1.i2.Impl;
with CORBA;
pragma Elaborate_All (CORBA);
with PortableServer;
pragma Elaborate_All (PortableServer);
with PolyORB.CORBA_P.Exceptions;

package body m1.i2.Skel is

   --  Skeleton subprograms

   function Servant_Is_A
     (Obj : PortableServer.Servant)
     return Boolean;

   function Servant_Is_A
     (Obj : PortableServer.Servant)
     return Boolean is
   begin
      return Obj.all in m1.i2.Impl.Object'Class;
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
               Result_Ü := m1.i2.Is_A
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

      elsif Operation = "_get_attr1" then

         declare
            Result_Ü      : m1.i1.t1;
         begin

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any


               --  Call implementation
               Result_Ü := m1.i2.Impl.get_attr1
                 (m1.i2.Impl.Object'Class (Self.all)'Access);
            end;

            -- Set result

            CORBA.ServerRequest.Set_Result
              (Request, 
               m1.i1.Helper.To_Any (Result_Ü));
            return;
         end;

      elsif Operation = "_set_attr1" then

         declare
            To            : m1.i1.t1;
            Arg_Name_Ü_To : constant CORBA.Identifier :=
              CORBA.To_CORBA_String ("To");
            Argument_Ü_To : CORBA.Any := CORBA.Internals.Get_Empty_Any
              (m1.i1.Helper.TC_t1);

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
                 m1.i1.Helper.From_Any (Argument_Ü_To);

               --  Call implementation
               m1.i2.Impl.set_attr1
                 (m1.i2.Impl.Object'Class (Self.all)'Access,
                  To);
            end;
            return;
         end;

      elsif Operation = "_get_bool" then

         declare
            Result_Ü      : m1.b1;
         begin

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any


               --  Call implementation
               Result_Ü := m1.i2.Impl.get_bool
                 (m1.i2.Impl.Object'Class (Self.all)'Access);
            end;

            -- Set result

            CORBA.ServerRequest.Set_Result
              (Request, 
               m1.Helper.To_Any (Result_Ü));
            return;
         end;

      elsif Operation = "_set_bool" then

         declare
            To            : m1.b1;
            Arg_Name_Ü_To : constant CORBA.Identifier :=
              CORBA.To_CORBA_String ("To");
            Argument_Ü_To : CORBA.Any := CORBA.Internals.Get_Empty_Any
              (m1.Helper.TC_b1);

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
                 m1.Helper.From_Any (Argument_Ü_To);

               --  Call implementation
               m1.i2.Impl.set_bool
                 (m1.i2.Impl.Object'Class (Self.all)'Access,
                  To);
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
        (CORBA.To_CORBA_String (m1.i2.Repository_Id),
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
         (Name      => +"m1.i2.Skel",
          Conflicts => Empty,
          Depends   =>
                  Empty
          ,
          Provides  => Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access));
   end;

end m1.i2.Skel;
