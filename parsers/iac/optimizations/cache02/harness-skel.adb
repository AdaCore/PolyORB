with Harness.Impl;
with CORBA;
with CORBA.NVList;
with CORBA.ServerRequest;
with PortableServer;
with CORBA.ORB;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Strings.Lists;
with PolyORB.Initialization;

package body Harness.Skel is

   function Servant_Is_A
     (Obj : in PortableServer.Servant)
     return Boolean
   is
   begin
      return Obj.all
         in Harness.Impl.Object'Class;
   end Servant_Is_A;

   Arg_Name_U_Arg : constant CORBA.Identifier
     := CORBA.To_CORBA_String
     ("arg");

   procedure Invoke
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr)
   is
      Operation_� : constant Standard.String :=
        CORBA.To_Standard_String
           (CORBA.ServerRequest.Operation
              (Request.all));

      Argument_List_� : CORBA.NVList.Ref;

      Argument_U_arg : CORBA.Any;

   begin
      Argument_U_arg := CORBA.Get_Empty_Any
        (CORBA.TC_Unsigned_Long);

      CORBA.ORB.Create_List
        (0,
         Argument_List_�);
      if Operation_�
         = "_Is_A"
      then
         declare
            Type_Id : CORBA.String;
            Arg_Name_U_Type_Id : constant CORBA.Identifier :=
              CORBA.To_CORBA_String
                 ("Type_Id");
            Argument_U_Type_Id : CORBA.Any :=
              CORBA.To_Any
                 (Type_Id);
            Result_� : CORBA.Boolean;
         begin
            CORBA.NVList.Add_Item
              (Argument_List_�,
               Arg_Name_U_Type_Id,
               Argument_U_Type_Id,
               CORBA.ARG_IN);
            CORBA.ServerRequest.Arguments
              (Request,
               Argument_List_�);
            Type_Id :=
              CORBA.From_Any
                 (Argument_U_Type_Id);
            Result_� :=
              Harness.Is_A
                 (CORBA.To_Standard_String
                    (Type_Id));
            CORBA.ServerRequest.Set_Result
              (Request,
               CORBA.To_Any
                 (Result_�));
            return;
         end;
      elsif Operation_�
         = "echoULong"
      then
         declare
            arg : CORBA.Unsigned_Long;

            Result_� : CORBA.Unsigned_Long;
         begin
            CORBA.NVList.Add_Item
              (Argument_List_�,
               Arg_Name_U_arg,
               Argument_U_arg,
               CORBA.ARG_IN);
            CORBA.ServerRequest.Arguments
              (Request,
               Argument_List_�);
            arg :=
              CORBA.From_Any
                 (Argument_U_arg);
            Result_� :=
              Harness.Impl.echoULong
                 (Harness.Impl.Object'Class
                    (Self.all)'Access,
                  arg);
            CORBA.ServerRequest.Set_Result
              (Request,
               CORBA.To_Any
                 (Result_�));
            return;
         end;
      else
         CORBA.Raise_Bad_Operation
           (CORBA.Default_Sys_Member);
      end if;
   end Invoke;

   procedure Deferred_Initialization

   is
   begin
      PortableServer.Register_Skeleton
        (CORBA.To_CORBA_String
           (Harness.Repository_Id),
         Servant_Is_A'Access,
         Invoke'Access);
   end Deferred_Initialization;

begin
   declare
      List : PolyORB.Utils.Strings.Lists.List;
   begin
      PolyORB.Utils.Strings.Lists.Append (List, "smart_pointers");
      PolyORB.Initialization.Register_Module
        (PolyORB.Initialization.Module_Info'
         (Name => PolyORB.Utils.Strings."+"
          ("Harness.Skel"),
          Conflicts => PolyORB.Utils.Strings.Lists.Empty,
          Depends => List,
          Provides => PolyORB.Utils.Strings.Lists.Empty,
          Implicit => False,
          Init => Deferred_Initialization'Access));
   end;
end Harness.Skel;
