pragma Style_Checks (Off);

with PolyORB.Utils.Strings;
with PolyORB.Initialization;
pragma Elaborate_All (PolyORB.Initialization);
with CORBA.Policy.Helper;
with CORBA.Helper;
with CORBA.ORB;
with CORBA.Object.Helper;
with CORBA.NVList;
with CORBA.ServerRequest;
with CORBA.DomainManager.Impl;
with CORBA;
pragma Elaborate_All (CORBA);
with PortableServer;
pragma Elaborate_All (PortableServer);
with PolyORB.CORBA_P.Exceptions;
with PolyORB.CORBA_P.IR_Hooks;
with PolyORB.CORBA_P.Domain_Management;

package body CORBA.DomainManager.Skel is

   --  Skeleton subprograms

   function Servant_Is_A
     (Obj : PortableServer.Servant)
     return Boolean;

   function Servant_Is_A
     (Obj : PortableServer.Servant)
     return Boolean is
   begin
      return Obj.all in CORBA.DomainManager.Impl.Object'Class;
   end Servant_Is_A;

   procedure Invoke
     (Self : PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr)
   is
      Operation : constant Standard.String
        := CORBA.To_Standard_String
        (CORBA.ServerRequest.Operation
         (Request.all));
      Arg_List_� : CORBA.NVList.Ref;
   begin
      CORBA.ORB.Create_List (0, Arg_List_�);
      if Operation = "_is_a" then
         declare
            Type_Id : CORBA.String;
            Arg_Name_�_Type_Id : constant CORBA.Identifier
              := CORBA.To_CORBA_String ("Type_Id");
            Argument_�_Type_Id : CORBA.Any := CORBA.To_Any (Type_Id);

            Result_� : CORBA.Boolean;
         begin
            CORBA.NVList.Add_Item
              (Arg_List_�,
               Arg_Name_�_Type_Id,
               Argument_�_Type_Id,
               CORBA.ARG_IN);

            CORBA.ServerRequest.Arguments (Request, Arg_List_�);

            begin
               --  Convert arguments from their Any

               Type_Id :=
                 CORBA.From_Any (Argument_�_Type_Id);

               --  Call implementation
               Result_� := CORBA.DomainManager.Is_A
                 (CORBA.To_Standard_String (Type_Id));
            end;

            -- Set Result

            CORBA.ServerRequest.Set_Result
              (Request,
               CORBA.To_Any (Result_�));
            return;
         end;

      elsif Operation = "_interface" then

         CORBA.ServerRequest.Arguments (Request, Arg_List_�);

         CORBA.ServerRequest.Set_Result
           (Request,
            CORBA.Object.Helper.To_Any
            (CORBA.Object.Ref
             (PolyORB.CORBA_P.IR_Hooks.Get_Interface_Definition
              (CORBA.To_CORBA_String (Repository_Id)))));

         return;

      elsif Operation = "_domain_managers" then

         CORBA.ServerRequest.Arguments (Request, Arg_List_�);

         CORBA.ServerRequest.Set_Result
           (Request,
            PolyORB.CORBA_P.Domain_Management.Get_Domain_Managers
            (Self));

         return;

      elsif Operation = "get_domain_policy" then

         declare
            Policy_Type            : CORBA.PolicyType;
            Arg_Name_�_Policy_Type : constant CORBA.Identifier :=
              CORBA.To_CORBA_String ("policy_type");
            Argument_�_Policy_Type : CORBA.Any := CORBA.Internals.Get_Empty_Any
              (CORBA.Helper.TC_PolicyType);

            Result_�               : CORBA.Policy.Ref;
         begin
            CORBA.NVList.Add_Item
              (Arg_List_�,
               Arg_Name_�_Policy_Type,
               Argument_�_Policy_Type,
               CORBA.ARG_IN);

            CORBA.ServerRequest.Arguments (Request, Arg_List_�);

            begin
               --  Convert arguments from their Any

               Policy_Type :=
                 CORBA.Helper.From_Any (Argument_�_Policy_Type);

               --  Call implementation
               Result_� := CORBA.DomainManager.Impl.Get_Domain_Policy
                 (CORBA.DomainManager.Impl.Object'Class (Self.all)'Access,
                  Policy_Type);
            end;

            -- Set result

            CORBA.ServerRequest.Set_Result
              (Request,
               CORBA.Policy.Helper.To_Any (Result_�));
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
               CORBA.Internals.To_CORBA_Any (PolyORB.CORBA_P.Exceptions.SystEm_Exception_To_Any (E)));
            return;
         end;
   end Invoke;

   procedure Deferred_Initialization is
   begin
      PortableServer.Internals.Register_Skeleton
        (CORBA.To_CORBA_String (CORBA.DomainManager.Repository_Id),
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
         (Name      => +"CORBA.DomainManager.Skel",
          Conflicts => Empty,
          Depends   =>
            Empty
          ,
          Provides  => Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access));
   end;

end CORBA.DomainManager.Skel;

