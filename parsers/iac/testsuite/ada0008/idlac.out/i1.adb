-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.adacore.com/polyorb/)
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
-------------------------------------------------
pragma Style_Checks (Off);

with i1.Helper;
with PolyORB.Types;
with PolyORB.Requests;
with PolyORB.Any.NVList;
with PolyORB.CORBA_P.Interceptors_Hooks;
with PolyORB.CORBA_P.Exceptions;
with CORBA.Object;
with CORBA;
 use CORBA;
pragma Elaborate_All (CORBA);

package body i1 is

   function get_str
     (Self : Ref)
     return CORBA.String
   is
      --  Prepare in arguments


      Operation_Name_� : constant Standard.String
        := "_get_str";
      Self_Ref_� : CORBA.Object.Ref
        := CORBA.Object.Ref (Self);

      Request_� : PolyORB.Requests.Request_Access;
      Arg_List_� : PolyORB.Any.NVList.Ref;
      Result_� : PolyORB.Any.NamedValue;
      Result_Name_� : CORBA.String := To_CORBA_String ("Result");
   begin

      if CORBA.Object.Is_Nil (Self_Ref_�) then
         CORBA.Raise_Inv_Objref (Default_Sys_Member);
      end if;

      --  Create argument list
      PolyORB.Any.NVList.Create
        (Arg_List_�);

      --  Set result type (maybe void)

      Result_�
        := (Name => PolyORB.Types.Identifier (Result_Name_�),
            Argument => CORBA.Internals.To_PolyORB_Any 
        (CORBA.Internals.Get_Empty_Any (CORBA.TC_String)),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => CORBA.Object.Internals.To_PolyORB_Ref
           (CORBA.Object.Ref (Self)),
         Operation => Operation_Name_�,
         Arg_List  => Arg_List_�,
         Result    => Result_�,
         Req       => Request_�);

      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags (0));
      if not PolyORB.Any.Is_Empty (Request_�.Exception_Info) then
         Result_�.Argument := Request_�.Exception_Info;
         PolyORB.Requests.Destroy_Request (Request_�);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any (Result_�.Argument);

         --  Not reached

      end if;
      PolyORB.Requests.Destroy_Request (Request_�);

      --  Request has been synchronously invoked.

      --  Retrieve return value

      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any (Result_�.Argument));
   end get_str;

   procedure set_str
     (Self : Ref;
      To : in CORBA.String)
   is
      --  Prepare in arguments

      Arg_Name_�_To : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("To");
      Argument_�_To : CORBA.Any
        := CORBA.To_Any
        (To);
      Operation_Name_� : constant Standard.String
        := "_set_str";
      Self_Ref_� : CORBA.Object.Ref
        := CORBA.Object.Ref (Self);

      Request_� : PolyORB.Requests.Request_Access;
      Arg_List_� : PolyORB.Any.NVList.Ref;
      Result_� : PolyORB.Any.NamedValue;
      Result_Name_� : CORBA.String := To_CORBA_String ("Result");
   begin

      if CORBA.Object.Is_Nil (Self_Ref_�) then
         CORBA.Raise_Inv_Objref (Default_Sys_Member);
      end if;

      --  Create argument list
      PolyORB.Any.NVList.Create
        (Arg_List_�);
      PolyORB.Any.NVList.Add_Item
        (Arg_List_�,
         Arg_Name_�_To,
         CORBA.Internals.To_PolyORB_Any (Argument_�_To),
         PolyORB.Any.ARG_IN);

      --  Set result type (maybe void)

      Result_�
        := (Name => PolyORB.Types.Identifier (Result_Name_�),
            Argument => CORBA.Internals.To_PolyORB_Any 
        (CORBA.Internals.Get_Empty_Any (CORBA.TC_Void)),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => CORBA.Object.Internals.To_PolyORB_Ref
           (CORBA.Object.Ref (Self)),
         Operation => Operation_Name_�,
         Arg_List  => Arg_List_�,
         Result    => Result_�,
         Req       => Request_�);

      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags (0));
      if not PolyORB.Any.Is_Empty (Request_�.Exception_Info) then
         Result_�.Argument := Request_�.Exception_Info;
         PolyORB.Requests.Destroy_Request (Request_�);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any (Result_�.Argument);

         --  Not reached

      end if;
      PolyORB.Requests.Destroy_Request (Request_�);

      --  Request has been synchronously invoked.
   end set_str;

   function get_S
     (Self : Ref)
     return i1.new_string
   is
      --  Prepare in arguments


      Operation_Name_� : constant Standard.String
        := "_get_S";
      Self_Ref_� : CORBA.Object.Ref
        := CORBA.Object.Ref (Self);

      Request_� : PolyORB.Requests.Request_Access;
      Arg_List_� : PolyORB.Any.NVList.Ref;
      Result_� : PolyORB.Any.NamedValue;
      Result_Name_� : CORBA.String := To_CORBA_String ("Result");
   begin

      if CORBA.Object.Is_Nil (Self_Ref_�) then
         CORBA.Raise_Inv_Objref (Default_Sys_Member);
      end if;

      --  Create argument list
      PolyORB.Any.NVList.Create
        (Arg_List_�);

      --  Set result type (maybe void)

      Result_�
        := (Name => PolyORB.Types.Identifier (Result_Name_�),
            Argument => CORBA.Internals.To_PolyORB_Any 
        (CORBA.Internals.Get_Empty_Any (i1.Helper.TC_new_string)),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => CORBA.Object.Internals.To_PolyORB_Ref
           (CORBA.Object.Ref (Self)),
         Operation => Operation_Name_�,
         Arg_List  => Arg_List_�,
         Result    => Result_�,
         Req       => Request_�);

      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags (0));
      if not PolyORB.Any.Is_Empty (Request_�.Exception_Info) then
         Result_�.Argument := Request_�.Exception_Info;
         PolyORB.Requests.Destroy_Request (Request_�);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any (Result_�.Argument);

         --  Not reached

      end if;
      PolyORB.Requests.Destroy_Request (Request_�);

      --  Request has been synchronously invoked.

      --  Retrieve return value

      return i1.Helper.From_Any
        (CORBA.Internals.To_CORBA_Any (Result_�.Argument));
   end get_S;

   procedure set_S
     (Self : Ref;
      To : in i1.new_string)
   is
      --  Prepare in arguments

      Arg_Name_�_To : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("To");
      Argument_�_To : CORBA.Any
        := i1.Helper.To_Any
        (To);
      Operation_Name_� : constant Standard.String
        := "_set_S";
      Self_Ref_� : CORBA.Object.Ref
        := CORBA.Object.Ref (Self);

      Request_� : PolyORB.Requests.Request_Access;
      Arg_List_� : PolyORB.Any.NVList.Ref;
      Result_� : PolyORB.Any.NamedValue;
      Result_Name_� : CORBA.String := To_CORBA_String ("Result");
   begin

      if CORBA.Object.Is_Nil (Self_Ref_�) then
         CORBA.Raise_Inv_Objref (Default_Sys_Member);
      end if;

      --  Create argument list
      PolyORB.Any.NVList.Create
        (Arg_List_�);
      PolyORB.Any.NVList.Add_Item
        (Arg_List_�,
         Arg_Name_�_To,
         CORBA.Internals.To_PolyORB_Any (Argument_�_To),
         PolyORB.Any.ARG_IN);

      --  Set result type (maybe void)

      Result_�
        := (Name => PolyORB.Types.Identifier (Result_Name_�),
            Argument => CORBA.Internals.To_PolyORB_Any 
        (CORBA.Internals.Get_Empty_Any (CORBA.TC_Void)),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => CORBA.Object.Internals.To_PolyORB_Ref
           (CORBA.Object.Ref (Self)),
         Operation => Operation_Name_�,
         Arg_List  => Arg_List_�,
         Result    => Result_�,
         Req       => Request_�);

      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags (0));
      if not PolyORB.Any.Is_Empty (Request_�.Exception_Info) then
         Result_�.Argument := Request_�.Exception_Info;
         PolyORB.Requests.Destroy_Request (Request_�);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any (Result_�.Argument);

         --  Not reached

      end if;
      PolyORB.Requests.Destroy_Request (Request_�);

      --  Request has been synchronously invoked.
   end set_S;

   procedure min
     (Self : Ref;
      f1 : in i1.New_Float)
   is
      --  Prepare in arguments

      Arg_Name_�_f1 : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("f1");
      Argument_�_f1 : CORBA.Any
        := i1.Helper.To_Any
        (f1);
      Operation_Name_� : constant Standard.String
        := "min";
      Self_Ref_� : CORBA.Object.Ref
        := CORBA.Object.Ref (Self);

      Request_� : PolyORB.Requests.Request_Access;
      Arg_List_� : PolyORB.Any.NVList.Ref;
      Result_� : PolyORB.Any.NamedValue;
      Result_Name_� : CORBA.String := To_CORBA_String ("Result");
   begin

      if CORBA.Object.Is_Nil (Self_Ref_�) then
         CORBA.Raise_Inv_Objref (Default_Sys_Member);
      end if;

      --  Create argument list
      PolyORB.Any.NVList.Create
        (Arg_List_�);
      PolyORB.Any.NVList.Add_Item
        (Arg_List_�,
         Arg_Name_�_f1,
         CORBA.Internals.To_PolyORB_Any (Argument_�_f1),
         PolyORB.Any.ARG_IN);

      --  Set result type (maybe void)

      Result_�
        := (Name => PolyORB.Types.Identifier (Result_Name_�),
            Argument => CORBA.Internals.To_PolyORB_Any 
        (CORBA.Internals.Get_Empty_Any (CORBA.TC_Void)),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => CORBA.Object.Internals.To_PolyORB_Ref
           (CORBA.Object.Ref (Self)),
         Operation => Operation_Name_�,
         Arg_List  => Arg_List_�,
         Result    => Result_�,
         Req       => Request_�);

      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags (0));
      if not PolyORB.Any.Is_Empty (Request_�.Exception_Info) then
         Result_�.Argument := Request_�.Exception_Info;
         PolyORB.Requests.Destroy_Request (Request_�);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any (Result_�.Argument);

         --  Not reached

      end if;
      PolyORB.Requests.Destroy_Request (Request_�);

      --  Request has been synchronously invoked.
   end min;

   procedure Add
     (Self : Ref;
      f1 : in out i1.New_Float;
      f2 : in CORBA.Float;
      Returns : out CORBA.Float)
   is
      --  Prepare in arguments

      Arg_Name_�_f1 : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("f1");
      Argument_�_f1 : CORBA.Any
        := i1.Helper.To_Any
        (f1);Arg_Name_�_f2 : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("f2");
      Argument_�_f2 : CORBA.Any
        := CORBA.To_Any
        (f2);
      Operation_Name_� : constant Standard.String
        := "Add";
      Self_Ref_� : CORBA.Object.Ref
        := CORBA.Object.Ref (Self);

      Request_� : PolyORB.Requests.Request_Access;
      Arg_List_� : PolyORB.Any.NVList.Ref;
      Result_� : PolyORB.Any.NamedValue;
      Result_Name_� : CORBA.String := To_CORBA_String ("Result");
   begin

      if CORBA.Object.Is_Nil (Self_Ref_�) then
         CORBA.Raise_Inv_Objref (Default_Sys_Member);
      end if;

      --  Create argument list
      PolyORB.Any.NVList.Create
        (Arg_List_�);
      PolyORB.Any.NVList.Add_Item
        (Arg_List_�,
         Arg_Name_�_f1,
         CORBA.Internals.To_PolyORB_Any (Argument_�_f1),
         PolyORB.Any.ARG_INOUT);
      PolyORB.Any.NVList.Add_Item
        (Arg_List_�,
         Arg_Name_�_f2,
         CORBA.Internals.To_PolyORB_Any (Argument_�_f2),
         PolyORB.Any.ARG_IN);

      --  Set result type (maybe void)

      Result_�
        := (Name => PolyORB.Types.Identifier (Result_Name_�),
            Argument => CORBA.Internals.To_PolyORB_Any 
        (CORBA.Internals.Get_Empty_Any (CORBA.TC_Float)),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => CORBA.Object.Internals.To_PolyORB_Ref
           (CORBA.Object.Ref (Self)),
         Operation => Operation_Name_�,
         Arg_List  => Arg_List_�,
         Result    => Result_�,
         Req       => Request_�);

      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags (0));
      if not PolyORB.Any.Is_Empty (Request_�.Exception_Info) then
         Result_�.Argument := Request_�.Exception_Info;
         PolyORB.Requests.Destroy_Request (Request_�);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any (Result_�.Argument);

         --  Not reached

      end if;
      PolyORB.Requests.Destroy_Request (Request_�);

      --  Request has been synchronously invoked.

      --  Retrieve return value

      Returns := CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any (Result_�.Argument));

      --  Retrieve out argument values.

      f1 := i1.Helper.From_Any
        (Argument_�_f1);
   end Add;

   procedure minus
     (Self : Ref;
      f1 : in CORBA.Float;
      f2 : in CORBA.Float;
      r : out CORBA.Float;
      Returns : out i1.New_Float)
   is
      --  Prepare in arguments

      Arg_Name_�_f1 : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("f1");
      Argument_�_f1 : CORBA.Any
        := CORBA.To_Any
        (f1);Arg_Name_�_f2 : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("f2");
      Argument_�_f2 : CORBA.Any
        := CORBA.To_Any
        (f2);Arg_Name_�_r : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("r");
      Argument_�_r : CORBA.Any
        := CORBA.Internals.Get_Empty_Any
         (CORBA.TC_Float);

      Operation_Name_� : constant Standard.String
        := "minus";
      Self_Ref_� : CORBA.Object.Ref
        := CORBA.Object.Ref (Self);

      Request_� : PolyORB.Requests.Request_Access;
      Arg_List_� : PolyORB.Any.NVList.Ref;
      Result_� : PolyORB.Any.NamedValue;
      Result_Name_� : CORBA.String := To_CORBA_String ("Result");
   begin

      if CORBA.Object.Is_Nil (Self_Ref_�) then
         CORBA.Raise_Inv_Objref (Default_Sys_Member);
      end if;

      --  Create argument list
      PolyORB.Any.NVList.Create
        (Arg_List_�);
      PolyORB.Any.NVList.Add_Item
        (Arg_List_�,
         Arg_Name_�_f1,
         CORBA.Internals.To_PolyORB_Any (Argument_�_f1),
         PolyORB.Any.ARG_IN);
      PolyORB.Any.NVList.Add_Item
        (Arg_List_�,
         Arg_Name_�_f2,
         CORBA.Internals.To_PolyORB_Any (Argument_�_f2),
         PolyORB.Any.ARG_IN);
      PolyORB.Any.NVList.Add_Item
        (Arg_List_�,
         Arg_Name_�_r,
         CORBA.Internals.To_PolyORB_Any (Argument_�_r),
         PolyORB.Any.ARG_OUT);

      --  Set result type (maybe void)

      Result_�
        := (Name => PolyORB.Types.Identifier (Result_Name_�),
            Argument => CORBA.Internals.To_PolyORB_Any 
        (CORBA.Internals.Get_Empty_Any (i1.Helper.TC_New_Float)),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => CORBA.Object.Internals.To_PolyORB_Ref
           (CORBA.Object.Ref (Self)),
         Operation => Operation_Name_�,
         Arg_List  => Arg_List_�,
         Result    => Result_�,
         Req       => Request_�);

      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags (0));
      if not PolyORB.Any.Is_Empty (Request_�.Exception_Info) then
         Result_�.Argument := Request_�.Exception_Info;
         PolyORB.Requests.Destroy_Request (Request_�);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any (Result_�.Argument);

         --  Not reached

      end if;
      PolyORB.Requests.Destroy_Request (Request_�);

      --  Request has been synchronously invoked.

      --  Retrieve return value

      Returns := i1.Helper.From_Any
        (CORBA.Internals.To_CORBA_Any (Result_�.Argument));

      --  Retrieve out argument values.

      r := CORBA.From_Any
        (Argument_�_r);
   end minus;

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
         i1.Repository_Id)
        or else CORBA.Is_Equivalent
          (Logical_Type_Id,
           "IDL:omg.org/CORBA/Object:1.0")
        or else False;

   end Is_A;

end i1;
