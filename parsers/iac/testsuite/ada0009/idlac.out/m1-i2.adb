-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.act-europe.fr/polyorb/)
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
-------------------------------------------------
pragma Style_Checks (Off);

with m1.Helper;
with m1.i1.Helper;
with PolyORB.Types;
with PolyORB.Requests;
with PolyORB.Any.NVList;
with PolyORB.CORBA_P.Interceptors_Hooks;
with PolyORB.CORBA_P.Exceptions;
with m1.i1;
with CORBA.Object;
with CORBA;
 use CORBA;
pragma Elaborate_All (CORBA);

package body m1.i2 is

   function get_attr1
     (Self : Ref)
     return m1.i1.t1
   is

      Operation_Name_Ü : constant Standard.String
        := "_get_attr1";
      Self_Ref_Ü : CORBA.Object.Ref
        := CORBA.Object.Ref (Self);

      Request_Ü : PolyORB.Requests.Request_Access;
      Arg_List_Ü : PolyORB.Any.NVList.Ref;
      Result_Ü : PolyORB.Any.NamedValue;
      Result_Name_Ü : CORBA.String := To_CORBA_String ("Result");
   begin

      if CORBA.Object.Is_Nil (Self_Ref_Ü) then
         CORBA.Raise_Inv_Objref (Default_Sys_Member);
      end if;

      --  Create argument list
      PolyORB.Any.NVList.Create
        (Arg_List_Ü);
      --  Set result type (maybe void)
      Result_Ü
        := (Name => PolyORB.Types.Identifier (Result_Name_Ü),
            Argument => CORBA.Internals.To_PolyORB_Any 
        (Get_Empty_Any (m1.i1.Helper.TC_t1)),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => CORBA.Object.To_PolyORB_Ref
         (CORBA.Object.Ref (Self)),
         Operation => Operation_Name_Ü,
         Arg_List  => Arg_List_Ü,
         Result    => Result_Ü,
         Req       => Request_Ü);

      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_Ü,
         PolyORB.Requests.Flags (0));
      if not PolyORB.Any.Is_Empty (Request_Ü.Exception_Info) then
         Result_Ü.Argument := Request_Ü.Exception_Info;
         PolyORB.Requests.Destroy_Request
           (Request_Ü);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any
           (Result_Ü.Argument);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request_Ü);

      --  Request has been synchronously invoked.

      --  Retrieve return value.
      return m1.i1.Helper.From_Any
        (CORBA.Internals.To_CORBA_Any (Result_Ü.Argument));
   end get_attr1;

   procedure set_attr1
     (Self : Ref;
      To : in m1.i1.t1)
   is
      Arg_Name_Ü_To : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("To");
      Argument_Ü_To : CORBA.Any
        := m1.i1.Helper.To_Any
        (To);

      Operation_Name_Ü : constant Standard.String
        := "_set_attr1";
      Self_Ref_Ü : CORBA.Object.Ref
        := CORBA.Object.Ref (Self);

      Request_Ü : PolyORB.Requests.Request_Access;
      Arg_List_Ü : PolyORB.Any.NVList.Ref;
      Result_Ü : PolyORB.Any.NamedValue;
      Result_Name_Ü : CORBA.String := To_CORBA_String ("Result");
   begin

      if CORBA.Object.Is_Nil (Self_Ref_Ü) then
         CORBA.Raise_Inv_Objref (Default_Sys_Member);
      end if;

      --  Create argument list
      PolyORB.Any.NVList.Create
        (Arg_List_Ü);
      PolyORB.Any.NVList.Add_Item
        (Arg_List_Ü,
         Arg_Name_Ü_To,
         CORBA.Internals.To_PolyORB_Any (Argument_Ü_To),
         PolyORB.Any.ARG_IN);
      --  Set result type (maybe void)
      Result_Ü
        := (Name => PolyORB.Types.Identifier (Result_Name_Ü),
            Argument => CORBA.Internals.To_PolyORB_Any 
        (Get_Empty_Any (CORBA.TC_Void)),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => CORBA.Object.To_PolyORB_Ref
         (CORBA.Object.Ref (Self)),
         Operation => Operation_Name_Ü,
         Arg_List  => Arg_List_Ü,
         Result    => Result_Ü,
         Req       => Request_Ü);

      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_Ü,
         PolyORB.Requests.Flags (0));
      if not PolyORB.Any.Is_Empty (Request_Ü.Exception_Info) then
         Result_Ü.Argument := Request_Ü.Exception_Info;
         PolyORB.Requests.Destroy_Request
           (Request_Ü);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any
           (Result_Ü.Argument);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request_Ü);

      --  Request has been synchronously invoked.
   end set_attr1;

   function get_bool
     (Self : Ref)
     return m1.b1
   is

      Operation_Name_Ü : constant Standard.String
        := "_get_bool";
      Self_Ref_Ü : CORBA.Object.Ref
        := CORBA.Object.Ref (Self);

      Request_Ü : PolyORB.Requests.Request_Access;
      Arg_List_Ü : PolyORB.Any.NVList.Ref;
      Result_Ü : PolyORB.Any.NamedValue;
      Result_Name_Ü : CORBA.String := To_CORBA_String ("Result");
   begin

      if CORBA.Object.Is_Nil (Self_Ref_Ü) then
         CORBA.Raise_Inv_Objref (Default_Sys_Member);
      end if;

      --  Create argument list
      PolyORB.Any.NVList.Create
        (Arg_List_Ü);
      --  Set result type (maybe void)
      Result_Ü
        := (Name => PolyORB.Types.Identifier (Result_Name_Ü),
            Argument => CORBA.Internals.To_PolyORB_Any 
        (Get_Empty_Any (m1.Helper.TC_b1)),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => CORBA.Object.To_PolyORB_Ref
         (CORBA.Object.Ref (Self)),
         Operation => Operation_Name_Ü,
         Arg_List  => Arg_List_Ü,
         Result    => Result_Ü,
         Req       => Request_Ü);

      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_Ü,
         PolyORB.Requests.Flags (0));
      if not PolyORB.Any.Is_Empty (Request_Ü.Exception_Info) then
         Result_Ü.Argument := Request_Ü.Exception_Info;
         PolyORB.Requests.Destroy_Request
           (Request_Ü);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any
           (Result_Ü.Argument);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request_Ü);

      --  Request has been synchronously invoked.

      --  Retrieve return value.
      return m1.Helper.From_Any
        (CORBA.Internals.To_CORBA_Any (Result_Ü.Argument));
   end get_bool;

   procedure set_bool
     (Self : Ref;
      To : in m1.b1)
   is
      Arg_Name_Ü_To : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("To");
      Argument_Ü_To : CORBA.Any
        := m1.Helper.To_Any
        (To);

      Operation_Name_Ü : constant Standard.String
        := "_set_bool";
      Self_Ref_Ü : CORBA.Object.Ref
        := CORBA.Object.Ref (Self);

      Request_Ü : PolyORB.Requests.Request_Access;
      Arg_List_Ü : PolyORB.Any.NVList.Ref;
      Result_Ü : PolyORB.Any.NamedValue;
      Result_Name_Ü : CORBA.String := To_CORBA_String ("Result");
   begin

      if CORBA.Object.Is_Nil (Self_Ref_Ü) then
         CORBA.Raise_Inv_Objref (Default_Sys_Member);
      end if;

      --  Create argument list
      PolyORB.Any.NVList.Create
        (Arg_List_Ü);
      PolyORB.Any.NVList.Add_Item
        (Arg_List_Ü,
         Arg_Name_Ü_To,
         CORBA.Internals.To_PolyORB_Any (Argument_Ü_To),
         PolyORB.Any.ARG_IN);
      --  Set result type (maybe void)
      Result_Ü
        := (Name => PolyORB.Types.Identifier (Result_Name_Ü),
            Argument => CORBA.Internals.To_PolyORB_Any 
        (Get_Empty_Any (CORBA.TC_Void)),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => CORBA.Object.To_PolyORB_Ref
         (CORBA.Object.Ref (Self)),
         Operation => Operation_Name_Ü,
         Arg_List  => Arg_List_Ü,
         Result    => Result_Ü,
         Req       => Request_Ü);

      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_Ü,
         PolyORB.Requests.Flags (0));
      if not PolyORB.Any.Is_Empty (Request_Ü.Exception_Info) then
         Result_Ü.Argument := Request_Ü.Exception_Info;
         PolyORB.Requests.Destroy_Request
           (Request_Ü);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any
           (Result_Ü.Argument);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request_Ü);

      --  Request has been synchronously invoked.
   end set_bool;

   --  The visible Is_A object reference
   --  operation (a dispatching operation
   --  of all object reference types).

   function Is_A
     (Self : Ref;
      Logical_Type_Id : Standard.String)
     return CORBA.Boolean
   is
   begin
      return False

        or else Is_A (Logical_Type_Id)
         --  Locally check class membership for this interface

        or else CORBA.Object.Is_A
                 (CORBA.Object.Ref (Self), Logical_Type_Id);
         --  Fall back to a remote membership check (may involve
         --  an actual request invocation on Self).

   end Is_A;

   --  The internal Is_A implementation for
   --  this interface.

   function Is_A
     (Logical_Type_Id : Standard.String)
     return CORBA.Boolean
   is
   begin
      return CORBA.Is_Equivalent
        (Logical_Type_Id,
         m1.i2.Repository_Id)
        or else CORBA.Is_Equivalent
          (Logical_Type_Id,
           "IDL:omg.org/CORBA/Object:1.0")
        or else False;

   end Is_A;

end m1.i2;
