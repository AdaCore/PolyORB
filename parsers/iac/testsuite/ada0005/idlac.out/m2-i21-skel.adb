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
with m2.I21.Helper;
with CORBA.ORB;
with CORBA.NVList;
with CORBA.ServerRequest;
with m2.I21.Impl;
with CORBA;
pragma Elaborate_All (CORBA);
with PortableServer;
pragma Elaborate_All (PortableServer);
with PolyORB.CORBA_P.Exceptions;

package body m2.I21.Skel is

   --  Skeleton subprograms

   function Servant_Is_A
     (Obj : PortableServer.Servant)
     return Boolean;

   function Servant_Is_A
     (Obj : PortableServer.Servant)
     return Boolean is
   begin
      return Obj.all in m2.I21.Impl.Object'Class;
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
               Result_Ü := m2.I21.Is_A
                 (CORBA.To_Standard_String (Type_Id));
            end;

            -- Set Result

            CORBA.ServerRequest.Set_Result
            (Request,
            CORBA.To_Any (
            Result_Ü));
            return;
         end;

      elsif Operation = "is_greater" then

         declare
            f1            : CORBA.Float;
            Arg_Name_Ü_f1 : constant CORBA.Identifier
              := CORBA.To_CORBA_String ("f1");
            Argument_Ü_f1 : CORBA.Any := CORBA.Get_Empty_Any
              (CORBA.TC_Float);

            f2            : CORBA.Float;
            Arg_Name_Ü_f2 : constant CORBA.Identifier
              := CORBA.To_CORBA_String ("f2");
            Argument_Ü_f2 : CORBA.Any := CORBA.Get_Empty_Any
              (CORBA.TC_Float);

            Result_Ü      : m2.I21.new_bool;
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

            CORBA.ServerRequest.Arguments (Request, Arg_List_Ü);

            begin
               --  Convert arguments from their Any

               f1 :=
                 CORBA.From_Any (Argument_Ü_f1);
               f2 :=
                 CORBA.From_Any (Argument_Ü_f2);

               --  Call implementation
               Result_Ü := m2.I21.Impl.is_greater
                 (m2.I21.Impl.Object'Class (Self.all)'Access,
                  f1,
                  f2);
            end;

            -- Set Result

            CORBA.ServerRequest.Set_Result
              (Request, 
               m2.I21.Helper.To_Any (
               Result_Ü));
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
        (CORBA.To_CORBA_String (m2.I21.Repository_Id),
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
         (Name      => +"m2.I21.Skel",
          Conflicts => Empty,
          Depends   =>
                  Empty
          ,
          Provides  => Empty,
          Implicit  => False,
          Init      => Deferred_Initialization'Access));
   end;

end m2.I21.Skel;
