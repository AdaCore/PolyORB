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
with i1.Helper;
with PolyORB.CORBA_P.Domain_Management;
with PolyORB.CORBA_P.IR_Hooks;
with CORBA.Object.Helper;
with CORBA.ORB;
with CORBA.NVList;
with CORBA.ServerRequest;
with i1.Impl;
with CORBA;
pragma Elaborate_All (CORBA);
with PortableServer;
pragma Elaborate_All (PortableServer);
with PolyORB.CORBA_P.Exceptions;

package body i1.Skel is

   --  Skeleton subprograms

   function Servant_Is_A
     (Obj : PortableServer.Servant)
     return Boolean;

   function Servant_Is_A
     (Obj : PortableServer.Servant)
     return Boolean is
   begin
      return Obj.all in i1.Impl.Object'Class;
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

               Result_Ü := i1.Is_A
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

      elsif Operation = "_get_str" then

         declare
            Result_Ü      : CORBA.String;
         begin

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any


               --  Call implementation

               Result_Ü := i1.Impl.get_str
                 (i1.Impl.Object'Class (Self.all)'Access);
            end;

            -- Set result

            CORBA.ServerRequest.Set_Result
              (Request, 
               CORBA.To_Any (Result_Ü));
            return;
         end;

      elsif Operation = "_set_str" then

         declare
            To            : CORBA.String;
            Arg_Name_Ü_To : constant CORBA.Identifier :=
              CORBA.To_CORBA_String ("To");
            Argument_Ü_To : CORBA.Any := CORBA.Internals.Get_Empty_Any
              (CORBA.TC_String);

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

               i1.Impl.set_str
                 (i1.Impl.Object'Class (Self.all)'Access,
                  To);
            end;
            return;
         end;

      elsif Operation = "_get_S" then

         declare
            Result_Ü      : i1.new_string;
         begin

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any


               --  Call implementation

               Result_Ü := i1.Impl.get_S
                 (i1.Impl.Object'Class (Self.all)'Access);
            end;

            -- Set result

            CORBA.ServerRequest.Set_Result
              (Request, 
               i1.Helper.To_Any (Result_Ü));
            return;
         end;

      elsif Operation = "_set_S" then

         declare
            To            : i1.new_string;
            Arg_Name_Ü_To : constant CORBA.Identifier :=
              CORBA.To_CORBA_String ("To");
            Argument_Ü_To : CORBA.Any := CORBA.Internals.Get_Empty_Any
              (i1.Helper.TC_new_string);

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
                 i1.Helper.From_Any (Argument_Ü_To);

               --  Call implementation

               i1.Impl.set_S
                 (i1.Impl.Object'Class (Self.all)'Access,
                  To);
            end;
            return;
         end;

      elsif Operation = "min" then

         declare
            f1            : i1.New_Float;
            Arg_Name_Ü_f1 : constant CORBA.Identifier :=
              CORBA.To_CORBA_String ("f1");
            Argument_Ü_f1 : CORBA.Any := CORBA.Internals.Get_Empty_Any
              (i1.Helper.TC_New_Float);

         begin
            CORBA.NVList.Add_Item
              (Arg_List_Ü,
               Arg_Name_Ü_f1,
               Argument_Ü_f1,
               CORBA.ARG_IN);

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any

               f1 :=
                 i1.Helper.From_Any (Argument_Ü_f1);

               --  Call implementation

               i1.Impl.min
                 (i1.Impl.Object'Class (Self.all)'Access,
                  f1);
            end;
            return;
         end;

      elsif Operation = "Add" then

         declare
            f1            : i1.New_Float;
            Arg_Name_Ü_f1 : constant CORBA.Identifier :=
              CORBA.To_CORBA_String ("f1");
            Argument_Ü_f1 : CORBA.Any := CORBA.Internals.Get_Empty_Any
              (i1.Helper.TC_New_Float);

            f2            : CORBA.Float;
            Arg_Name_Ü_f2 : constant CORBA.Identifier :=
              CORBA.To_CORBA_String ("f2");
            Argument_Ü_f2 : CORBA.Any := CORBA.Internals.Get_Empty_Any
              (CORBA.TC_Float);

            Returns       : CORBA.Float;
         begin
            CORBA.NVList.Add_Item
              (Arg_List_Ü,
               Arg_Name_Ü_f1,
               Argument_Ü_f1,
               CORBA.ARG_INOUT);
            CORBA.NVList.Add_Item
              (Arg_List_Ü,
               Arg_Name_Ü_f2,
               Argument_Ü_f2,
               CORBA.ARG_IN);

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any

               f1 :=
                 i1.Helper.From_Any (Argument_Ü_f1);
               f2 :=
                 CORBA.From_Any (Argument_Ü_f2);

               --  Call implementation

               i1.Impl.Add
                 (i1.Impl.Object'Class (Self.all)'Access,
                  f1,
                  f2,
                  Returns);
            end;

            --  Set out arguments.

            CORBA.Internals.Move_Any_Value
              (Argument_Ü_f1,
               i1.Helper.To_Any
                 (f1));

            -- Set result

            CORBA.ServerRequest.Set_Result
              (Request, 
               CORBA.To_Any (Returns));
            return;
         end;

      elsif Operation = "minus" then

         declare
            f1            : CORBA.Float;
            Arg_Name_Ü_f1 : constant CORBA.Identifier :=
              CORBA.To_CORBA_String ("f1");
            Argument_Ü_f1 : CORBA.Any := CORBA.Internals.Get_Empty_Any
              (CORBA.TC_Float);

            f2            : CORBA.Float;
            Arg_Name_Ü_f2 : constant CORBA.Identifier :=
              CORBA.To_CORBA_String ("f2");
            Argument_Ü_f2 : CORBA.Any := CORBA.Internals.Get_Empty_Any
              (CORBA.TC_Float);

            r             : CORBA.Float;
            Arg_Name_Ü_r  : constant CORBA.Identifier :=
              CORBA.To_CORBA_String ("r");
            Argument_Ü_r  : CORBA.Any := CORBA.Internals.Get_Empty_Any
              (CORBA.TC_Float);

            Returns       : i1.New_Float;
         begin
            CORBA.NVList.Add_Item
              (Arg_List_Ü,
               Arg_Name_Ü_f1,
               Argument_Ü_f1,
               CORBA.ARG_IN);
            CORBA.NVList.Add_Item
              (Arg_List_Ü,
               Arg_Name_Ü_f2,
               Argument_Ü_f2,
               CORBA.ARG_IN);
            CORBA.NVList.Add_Item
              (Arg_List_Ü,
               Arg_Name_Ü_r,
               Argument_Ü_r,
               CORBA.ARG_OUT);

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any

               f1 :=
                 CORBA.From_Any (Argument_Ü_f1);
               f2 :=
                 CORBA.From_Any (Argument_Ü_f2);

               --  Call implementation

               i1.Impl.minus
                 (i1.Impl.Object'Class (Self.all)'Access,
                  f1,
                  f2,
                  r,
                  Returns);
            end;

            --  Set out arguments.

            CORBA.Internals.Move_Any_Value
              (Argument_Ü_r,
               CORBA.To_Any
                 (r));

            -- Set result

            CORBA.ServerRequest.Set_Result
              (Request, 
               i1.Helper.To_Any (Returns));
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
        (CORBA.To_CORBA_String (i1.Repository_Id),
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
         (Name      => +"i1.Skel",
          Conflicts => Empty,
          Depends   =>
                  Empty
          ,
          Provides  => Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access));
   end;

end i1.Skel;
