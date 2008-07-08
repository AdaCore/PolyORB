
package harness_IDL_File is

end harness_IDL_File;

package body harness_IDL_File is

end harness_IDL_File;

package harness_IDL_File.Helper is

end harness_IDL_File.Helper;

package body harness_IDL_File.Helper is

end harness_IDL_File.Helper;
with CORBA.Object;
with CORBA;

package Harness is

   type Ref is
     new CORBA.Object.Ref with null record;

   Repository_Id : constant Standard.String :=
     "IDL:Harness:1.0";

   function echoULong
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long;

   function Is_A
     (Self : in Ref;
      Logical_Type_Id : in Standard.String)
     return CORBA.Boolean;

private
   function Is_A
     (Logical_Type_Id : in Standard.String)
     return CORBA.Boolean;

end Harness;
with PolyORB.Any.NVList;
with PolyORB.Types;
with CORBA;
with CORBA.Object;
with PolyORB.Requests;
with PolyORB.Any;
with PolyORB.CORBA_P.Interceptors_Hooks;
with PolyORB.CORBA_P.Exceptions;

package body Harness is

   echoULong_Arg_Name_U_arg : PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("arg");

   echoULong_Operation_Name_ü : constant Standard.String :=
     "echoULong";

   echoULong_Result_Name_ü : CORBA.String :=
     CORBA.To_CORBA_String
        ("Result");

   function echoULong
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Argument_List_ü : PolyORB.Any.NVList.Ref;
      Argument_U_arg : CORBA.Any :=
        CORBA.To_Any
           (arg);
      Self_Ref_ü : CORBA.Object.Ref :=
        CORBA.Object.Ref
           (Self);
      Request_ü : PolyORB.Requests.Request_Access;
      Result_ü : PolyORB.Any.NamedValue;
   begin
      if CORBA.Object.Is_Nil
        (Self_Ref_ü)
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PolyORB.Any.NVList.Create
        (Argument_List_ü);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_ü,
         echoULong_Arg_Name_U_arg,
         CORBA.Internals.To_PolyORB_Any
           (Argument_U_arg),
         PolyORB.Any.ARG_IN);
      Result_ü :=
        (Name => PolyORB.Types.Identifier
           (echoULong_Result_Name_ü),
         Argument => CORBA.Internals.To_PolyORB_Any
           (CORBA.Get_Empty_Any
              (CORBA.TC_Unsigned_Long)),
         Arg_Modes => 0);
      PolyORB.Requests.Create_Request
        (Target => CORBA.Object.To_PolyORB_Ref
           (CORBA.Object.Ref
              (Self)),
         Operation => echoULong_Operation_Name_ü,
         Arg_List => Argument_List_ü,
         Result => Result_ü,
         Req => Request_ü);
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_ü,
         PolyORB.Requests.Flags
           (0));
      if not PolyORB.Any.Is_Empty
        (Request_ü.Exception_Info)
      then
         Result_ü.Argument :=
           Request_ü.Exception_Info;
         PolyORB.Requests.Destroy_Request
           (Request_ü);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any
           (Result_ü.Argument);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request_ü);
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any
           (Result_ü.Argument));
   end echoULong;

   function Is_A
     (Self : in Ref;
      Logical_Type_Id : in Standard.String)
     return CORBA.Boolean
   is
   begin
      return False
         or else Is_A
           (Logical_Type_Id)
            or else CORBA.Object.Is_A
              (CORBA.Object.Ref
                 (Self),
               Logical_Type_Id);
   end Is_A;

   function Is_A
     (Logical_Type_Id : in Standard.String)
     return CORBA.Boolean
   is
   begin
      return CORBA.Is_Equivalent
        (Logical_Type_Id,
         Harness.Repository_Id)
         or else CORBA.Is_Equivalent
           (Logical_Type_Id,
            "IDL:omg.org/CORBA/Object:1.0")
            or else False;
   end Is_A;

end Harness;
with CORBA.Object;
with PolyORB.Any;
with CORBA;

package Harness.Helper is

   function To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
     return Harness.Ref;

   function Unchecked_To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
     return Harness.Ref;

   TC_Harness : CORBA.TypeCode.Object :=
     CORBA.TypeCode.Internals.To_CORBA_Object
        (PolyORB.Any.TypeCode.TC_Object);

   function From_Any
     (Item : in CORBA.Any)
     return Harness.Ref;

   function To_Any
     (Item : in Harness.Ref)
     return CORBA.Any;

end Harness.Helper;
with CORBA.Object;
with CORBA;
with CORBA.Object.Helper;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Strings.Lists;
with PolyORB.Initialization;

package body Harness.Helper is

   function To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
     return Harness.Ref
   is
   begin
      if CORBA.Object.Is_Nil
        (The_Ref)
         or else CORBA.Object.Is_A
           (The_Ref,
            Repository_Id)
      then
         return Unchecked_To_Ref
           (The_Ref);
      end if;
      CORBA.Raise_Bad_Param
        (CORBA.Default_Sys_Member);
   end To_Ref;

   function Unchecked_To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
     return Harness.Ref
   is
      Result : Harness.Ref;
   begin
      Set
        (Result,
         CORBA.Object.Object_Of
           (The_Ref));
      return Result;
   end Unchecked_To_Ref;

   function From_Any
     (Item : in CORBA.Any)
     return Harness.Ref
   is
   begin
      return To_Ref
        (CORBA.Object.Helper.From_Any
           (Item));
   end From_Any;

   function To_Any
     (Item : in Harness.Ref)
     return CORBA.Any
   is
      A : CORBA.Any :=
        CORBA.Object.Helper.To_Any
           (CORBA.Object.Ref
              (Item));
   begin
      CORBA.Set_Type
        (A,
         TC_Harness);
      return A;
   end To_Any;

   procedure Deferred_Initialization

   is
   begin
      declare
         Name_ü : CORBA.String :=
           CORBA.To_CORBA_String
              ("Harness");
         Id_ü : CORBA.String :=
           CORBA.To_CORBA_String
              ("IDL:Harness:1.0");
      begin
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_Harness,
            CORBA.To_Any
              (Name_ü));
         CORBA.TypeCode.Internals.Add_Parameter
           (TC_Harness,
            CORBA.To_Any
              (Id_ü));
      end;
   end Deferred_Initialization;

begin
   PolyORB.Initialization.Register_Module
     (PolyORB.Initialization.Module_Info'
        (Name => PolyORB.Utils.Strings."+"
           ("Harness.Helper"),
         Conflicts => PolyORB.Utils.Strings.Lists.Empty,
         Depends => PolyORB.Utils.Strings.Lists.Empty,
         Provides => PolyORB.Utils.Strings.Lists.Empty,
         Implicit => False,
         Init => Deferred_Initialization'Access));
end Harness.Helper;

package Harness.Skel is

   pragma Elaborate_Body;

end Harness.Skel;
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

   procedure Invoke
     (Self : in PortableServer.Servant;
      Request : in CORBA.ServerRequest.Object_Ptr)
   is
      Operation_ü : constant Standard.String :=
        CORBA.To_Standard_String
           (CORBA.ServerRequest.Operation
              (Request.all));
      Argument_List_ü : CORBA.NVList.Ref;
   begin
      CORBA.ORB.Create_List
        (0,
         Argument_List_ü);
      if Operation_ü
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
            Result_ü : CORBA.Boolean;
         begin
            CORBA.NVList.Add_Item
              (Argument_List_ü,
               Arg_Name_U_Type_Id,
               Argument_U_Type_Id,
               CORBA.ARG_IN);
            CORBA.ServerRequest.Arguments
              (Request,
               Argument_List_ü);
            Type_Id :=
              CORBA.From_Any
                 (Argument_U_Type_Id);
            Result_ü :=
              Harness.Is_A
                 (CORBA.To_Standard_String
                    (Type_Id));
            CORBA.ServerRequest.Set_Result
              (Request,
               CORBA.To_Any
                 (Result_ü));
            return;
         end;
      elsif Operation_ü
         = "echoULong"
      then
         declare
            arg : CORBA.Unsigned_Long;
            Arg_Name_U_arg : constant CORBA.Identifier :=
              CORBA.To_CORBA_String
                 ("arg");
            Argument_U_arg : CORBA.Any :=
              CORBA.Get_Empty_Any
                 (CORBA.TC_Unsigned_Long);
            Result_ü : CORBA.Unsigned_Long;
         begin
            CORBA.NVList.Add_Item
              (Argument_List_ü,
               Arg_Name_U_arg,
               Argument_U_arg,
               CORBA.ARG_IN);
            CORBA.ServerRequest.Arguments
              (Request,
               Argument_List_ü);
            arg :=
              CORBA.From_Any
                 (Argument_U_arg);
            Result_ü :=
              Harness.Impl.echoULong
                 (Harness.Impl.Object'Class
                    (Self.all)'Access,
                  arg);
            CORBA.ServerRequest.Set_Result
              (Request,
               CORBA.To_Any
                 (Result_ü));
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
   PolyORB.Initialization.Register_Module
     (PolyORB.Initialization.Module_Info'
        (Name => PolyORB.Utils.Strings."+"
           ("Harness.Skel"),
         Conflicts => PolyORB.Utils.Strings.Lists.Empty,
         Depends => PolyORB.Utils.Strings.Lists.Empty,
         Provides => PolyORB.Utils.Strings.Lists.Empty,
         Implicit => False,
         Init => Deferred_Initialization'Access));
end Harness.Skel;
