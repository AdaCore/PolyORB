with PolyORB.Any.NVList;
with PolyORB.Types;
with CORBA;
with CORBA.Object;
with PolyORB.Requests;
with PolyORB.Any;
with PolyORB.CORBA_P.Interceptors_Hooks;
with PolyORB.CORBA_P.Exceptions;

package body Harness is

   echoULong1_Arg_Name_U_arg : PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("arg");

   echoULong1_Operation_Name_� : constant Standard.String :=
     "echoULong1";

   echoULong1_Result_Name_� : CORBA.String :=
     CORBA.To_CORBA_String
        ("Result");

   function echoULong1_Result_�

     return PolyORB.Any.NamedValue
   is
      pragma Inline
        (echoULong1_Result_�);
   begin
      return (Name => PolyORB.Types.Identifier
        (echoULong1_Result_Name_�),
      Argument => CORBA.Internals.To_PolyORB_Any
        (CORBA.Get_Empty_Any
           (CORBA.TC_Unsigned_Long)),
      Arg_Modes => 0);
   end echoULong1_Result_�;

   function echoULong1
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Argument_U_arg : CORBA.Any :=
        CORBA.To_Any
           (arg);
      Self_Ref_� : CORBA.Object.Ref :=
        CORBA.Object.Ref
           (Self);
      Request_� : PolyORB.Requests.Request_Access;
      Result_� : PolyORB.Any.NamedValue :=
        echoULong1_Result_�;
   begin
      if CORBA.Object.Is_Nil
        (Self_Ref_�)
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PolyORB.Any.NVList.Create
        (Argument_List_�);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         echoULong1_Arg_Name_U_arg,
         CORBA.Internals.To_PolyORB_Any
           (Argument_U_arg),
         PolyORB.Any.ARG_IN);
      PolyORB.Requests.Create_Request
        (Target => CORBA.Object.To_PolyORB_Ref
           (CORBA.Object.Ref
              (Self)),
         Operation => echoULong1_Operation_Name_�,
         Arg_List => Argument_List_�,
         Result => Result_�,
         Req => Request_�);
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags
           (0));
      if not PolyORB.Any.Is_Empty
        (Request_�.Exception_Info)
      then
         Result_�.Argument :=
           Request_�.Exception_Info;
         PolyORB.Requests.Destroy_Request
           (Request_�);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any
           (Result_�.Argument);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request_�);
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any
           (Result_�.Argument));
   end echoULong1;

   echoULong2_Arg_Name_U_arg : PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("arg");

   echoULong2_Operation_Name_� : constant Standard.String :=
     "echoULong2";

   echoULong2_Result_Name_� : CORBA.String :=
     CORBA.To_CORBA_String
        ("Result");

   function echoULong2_Result_�

     return PolyORB.Any.NamedValue
   is
      pragma Inline
        (echoULong2_Result_�);
   begin
      return (Name => PolyORB.Types.Identifier
        (echoULong2_Result_Name_�),
      Argument => CORBA.Internals.To_PolyORB_Any
        (CORBA.Get_Empty_Any
           (CORBA.TC_Unsigned_Long)),
      Arg_Modes => 0);
   end echoULong2_Result_�;

   function echoULong2
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Argument_U_arg : CORBA.Any :=
        CORBA.To_Any
           (arg);
      Self_Ref_� : CORBA.Object.Ref :=
        CORBA.Object.Ref
           (Self);
      Request_� : PolyORB.Requests.Request_Access;
      Result_� : PolyORB.Any.NamedValue :=
        echoULong2_Result_�;
   begin
      if CORBA.Object.Is_Nil
        (Self_Ref_�)
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PolyORB.Any.NVList.Create
        (Argument_List_�);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         echoULong2_Arg_Name_U_arg,
         CORBA.Internals.To_PolyORB_Any
           (Argument_U_arg),
         PolyORB.Any.ARG_IN);
      PolyORB.Requests.Create_Request
        (Target => CORBA.Object.To_PolyORB_Ref
           (CORBA.Object.Ref
              (Self)),
         Operation => echoULong2_Operation_Name_�,
         Arg_List => Argument_List_�,
         Result => Result_�,
         Req => Request_�);
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags
           (0));
      if not PolyORB.Any.Is_Empty
        (Request_�.Exception_Info)
      then
         Result_�.Argument :=
           Request_�.Exception_Info;
         PolyORB.Requests.Destroy_Request
           (Request_�);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any
           (Result_�.Argument);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request_�);
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any
           (Result_�.Argument));
   end echoULong2;

   echoULong3_Arg_Name_U_arg : PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("arg");

   echoULong3_Operation_Name_� : constant Standard.String :=
     "echoULong3";

   echoULong3_Result_Name_� : CORBA.String :=
     CORBA.To_CORBA_String
        ("Result");

   function echoULong3_Result_�

     return PolyORB.Any.NamedValue
   is
      pragma Inline
        (echoULong3_Result_�);
   begin
      return (Name => PolyORB.Types.Identifier
        (echoULong3_Result_Name_�),
      Argument => CORBA.Internals.To_PolyORB_Any
        (CORBA.Get_Empty_Any
           (CORBA.TC_Unsigned_Long)),
      Arg_Modes => 0);
   end echoULong3_Result_�;

   function echoULong3
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Argument_U_arg : CORBA.Any :=
        CORBA.To_Any
           (arg);
      Self_Ref_� : CORBA.Object.Ref :=
        CORBA.Object.Ref
           (Self);
      Request_� : PolyORB.Requests.Request_Access;
      Result_� : PolyORB.Any.NamedValue :=
        echoULong3_Result_�;
   begin
      if CORBA.Object.Is_Nil
        (Self_Ref_�)
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PolyORB.Any.NVList.Create
        (Argument_List_�);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         echoULong3_Arg_Name_U_arg,
         CORBA.Internals.To_PolyORB_Any
           (Argument_U_arg),
         PolyORB.Any.ARG_IN);
      PolyORB.Requests.Create_Request
        (Target => CORBA.Object.To_PolyORB_Ref
           (CORBA.Object.Ref
              (Self)),
         Operation => echoULong3_Operation_Name_�,
         Arg_List => Argument_List_�,
         Result => Result_�,
         Req => Request_�);
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags
           (0));
      if not PolyORB.Any.Is_Empty
        (Request_�.Exception_Info)
      then
         Result_�.Argument :=
           Request_�.Exception_Info;
         PolyORB.Requests.Destroy_Request
           (Request_�);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any
           (Result_�.Argument);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request_�);
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any
           (Result_�.Argument));
   end echoULong3;

   echoULong4_Arg_Name_U_arg : PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("arg");

   echoULong4_Operation_Name_� : constant Standard.String :=
     "echoULong4";

   echoULong4_Result_Name_� : CORBA.String :=
     CORBA.To_CORBA_String
        ("Result");

   function echoULong4_Result_�

     return PolyORB.Any.NamedValue
   is
      pragma Inline
        (echoULong4_Result_�);
   begin
      return (Name => PolyORB.Types.Identifier
        (echoULong4_Result_Name_�),
      Argument => CORBA.Internals.To_PolyORB_Any
        (CORBA.Get_Empty_Any
           (CORBA.TC_Unsigned_Long)),
      Arg_Modes => 0);
   end echoULong4_Result_�;

   function echoULong4
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Argument_U_arg : CORBA.Any :=
        CORBA.To_Any
           (arg);
      Self_Ref_� : CORBA.Object.Ref :=
        CORBA.Object.Ref
           (Self);
      Request_� : PolyORB.Requests.Request_Access;
      Result_� : PolyORB.Any.NamedValue :=
        echoULong4_Result_�;
   begin
      if CORBA.Object.Is_Nil
        (Self_Ref_�)
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PolyORB.Any.NVList.Create
        (Argument_List_�);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         echoULong4_Arg_Name_U_arg,
         CORBA.Internals.To_PolyORB_Any
           (Argument_U_arg),
         PolyORB.Any.ARG_IN);
      PolyORB.Requests.Create_Request
        (Target => CORBA.Object.To_PolyORB_Ref
           (CORBA.Object.Ref
              (Self)),
         Operation => echoULong4_Operation_Name_�,
         Arg_List => Argument_List_�,
         Result => Result_�,
         Req => Request_�);
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags
           (0));
      if not PolyORB.Any.Is_Empty
        (Request_�.Exception_Info)
      then
         Result_�.Argument :=
           Request_�.Exception_Info;
         PolyORB.Requests.Destroy_Request
           (Request_�);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any
           (Result_�.Argument);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request_�);
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any
           (Result_�.Argument));
   end echoULong4;

   echoULong5_Arg_Name_U_arg : PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("arg");

   echoULong5_Operation_Name_� : constant Standard.String :=
     "echoULong5";

   echoULong5_Result_Name_� : CORBA.String :=
     CORBA.To_CORBA_String
        ("Result");

   function echoULong5_Result_�

     return PolyORB.Any.NamedValue
   is
      pragma Inline
        (echoULong5_Result_�);
   begin
      return (Name => PolyORB.Types.Identifier
        (echoULong5_Result_Name_�),
      Argument => CORBA.Internals.To_PolyORB_Any
        (CORBA.Get_Empty_Any
           (CORBA.TC_Unsigned_Long)),
      Arg_Modes => 0);
   end echoULong5_Result_�;

   function echoULong5
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Argument_U_arg : CORBA.Any :=
        CORBA.To_Any
           (arg);
      Self_Ref_� : CORBA.Object.Ref :=
        CORBA.Object.Ref
           (Self);
      Request_� : PolyORB.Requests.Request_Access;
      Result_� : PolyORB.Any.NamedValue :=
        echoULong5_Result_�;
   begin
      if CORBA.Object.Is_Nil
        (Self_Ref_�)
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PolyORB.Any.NVList.Create
        (Argument_List_�);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         echoULong5_Arg_Name_U_arg,
         CORBA.Internals.To_PolyORB_Any
           (Argument_U_arg),
         PolyORB.Any.ARG_IN);
      PolyORB.Requests.Create_Request
        (Target => CORBA.Object.To_PolyORB_Ref
           (CORBA.Object.Ref
              (Self)),
         Operation => echoULong5_Operation_Name_�,
         Arg_List => Argument_List_�,
         Result => Result_�,
         Req => Request_�);
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags
           (0));
      if not PolyORB.Any.Is_Empty
        (Request_�.Exception_Info)
      then
         Result_�.Argument :=
           Request_�.Exception_Info;
         PolyORB.Requests.Destroy_Request
           (Request_�);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any
           (Result_�.Argument);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request_�);
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any
           (Result_�.Argument));
   end echoULong5;

   echoULong6_Arg_Name_U_arg : PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("arg");

   echoULong6_Operation_Name_� : constant Standard.String :=
     "echoULong6";

   echoULong6_Result_Name_� : CORBA.String :=
     CORBA.To_CORBA_String
        ("Result");

   function echoULong6_Result_�

     return PolyORB.Any.NamedValue
   is
      pragma Inline
        (echoULong6_Result_�);
   begin
      return (Name => PolyORB.Types.Identifier
        (echoULong6_Result_Name_�),
      Argument => CORBA.Internals.To_PolyORB_Any
        (CORBA.Get_Empty_Any
           (CORBA.TC_Unsigned_Long)),
      Arg_Modes => 0);
   end echoULong6_Result_�;

   function echoULong6
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Argument_U_arg : CORBA.Any :=
        CORBA.To_Any
           (arg);
      Self_Ref_� : CORBA.Object.Ref :=
        CORBA.Object.Ref
           (Self);
      Request_� : PolyORB.Requests.Request_Access;
      Result_� : PolyORB.Any.NamedValue :=
        echoULong6_Result_�;
   begin
      if CORBA.Object.Is_Nil
        (Self_Ref_�)
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PolyORB.Any.NVList.Create
        (Argument_List_�);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         echoULong6_Arg_Name_U_arg,
         CORBA.Internals.To_PolyORB_Any
           (Argument_U_arg),
         PolyORB.Any.ARG_IN);
      PolyORB.Requests.Create_Request
        (Target => CORBA.Object.To_PolyORB_Ref
           (CORBA.Object.Ref
              (Self)),
         Operation => echoULong6_Operation_Name_�,
         Arg_List => Argument_List_�,
         Result => Result_�,
         Req => Request_�);
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags
           (0));
      if not PolyORB.Any.Is_Empty
        (Request_�.Exception_Info)
      then
         Result_�.Argument :=
           Request_�.Exception_Info;
         PolyORB.Requests.Destroy_Request
           (Request_�);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any
           (Result_�.Argument);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request_�);
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any
           (Result_�.Argument));
   end echoULong6;

   echoULong7_Arg_Name_U_arg : PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("arg");

   echoULong7_Operation_Name_� : constant Standard.String :=
     "echoULong7";

   echoULong7_Result_Name_� : CORBA.String :=
     CORBA.To_CORBA_String
        ("Result");

   function echoULong7_Result_�

     return PolyORB.Any.NamedValue
   is
      pragma Inline
        (echoULong7_Result_�);
   begin
      return (Name => PolyORB.Types.Identifier
        (echoULong7_Result_Name_�),
      Argument => CORBA.Internals.To_PolyORB_Any
        (CORBA.Get_Empty_Any
           (CORBA.TC_Unsigned_Long)),
      Arg_Modes => 0);
   end echoULong7_Result_�;

   function echoULong7
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Argument_U_arg : CORBA.Any :=
        CORBA.To_Any
           (arg);
      Self_Ref_� : CORBA.Object.Ref :=
        CORBA.Object.Ref
           (Self);
      Request_� : PolyORB.Requests.Request_Access;
      Result_� : PolyORB.Any.NamedValue :=
        echoULong7_Result_�;
   begin
      if CORBA.Object.Is_Nil
        (Self_Ref_�)
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PolyORB.Any.NVList.Create
        (Argument_List_�);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         echoULong7_Arg_Name_U_arg,
         CORBA.Internals.To_PolyORB_Any
           (Argument_U_arg),
         PolyORB.Any.ARG_IN);
      PolyORB.Requests.Create_Request
        (Target => CORBA.Object.To_PolyORB_Ref
           (CORBA.Object.Ref
              (Self)),
         Operation => echoULong7_Operation_Name_�,
         Arg_List => Argument_List_�,
         Result => Result_�,
         Req => Request_�);
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags
           (0));
      if not PolyORB.Any.Is_Empty
        (Request_�.Exception_Info)
      then
         Result_�.Argument :=
           Request_�.Exception_Info;
         PolyORB.Requests.Destroy_Request
           (Request_�);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any
           (Result_�.Argument);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request_�);
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any
           (Result_�.Argument));
   end echoULong7;

   echoULong8_Arg_Name_U_arg : PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("arg");

   echoULong8_Operation_Name_� : constant Standard.String :=
     "echoULong8";

   echoULong8_Result_Name_� : CORBA.String :=
     CORBA.To_CORBA_String
        ("Result");

   function echoULong8_Result_�

     return PolyORB.Any.NamedValue
   is
      pragma Inline
        (echoULong8_Result_�);
   begin
      return (Name => PolyORB.Types.Identifier
        (echoULong8_Result_Name_�),
      Argument => CORBA.Internals.To_PolyORB_Any
        (CORBA.Get_Empty_Any
           (CORBA.TC_Unsigned_Long)),
      Arg_Modes => 0);
   end echoULong8_Result_�;

   function echoULong8
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Argument_U_arg : CORBA.Any :=
        CORBA.To_Any
           (arg);
      Self_Ref_� : CORBA.Object.Ref :=
        CORBA.Object.Ref
           (Self);
      Request_� : PolyORB.Requests.Request_Access;
      Result_� : PolyORB.Any.NamedValue :=
        echoULong8_Result_�;
   begin
      if CORBA.Object.Is_Nil
        (Self_Ref_�)
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PolyORB.Any.NVList.Create
        (Argument_List_�);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         echoULong8_Arg_Name_U_arg,
         CORBA.Internals.To_PolyORB_Any
           (Argument_U_arg),
         PolyORB.Any.ARG_IN);
      PolyORB.Requests.Create_Request
        (Target => CORBA.Object.To_PolyORB_Ref
           (CORBA.Object.Ref
              (Self)),
         Operation => echoULong8_Operation_Name_�,
         Arg_List => Argument_List_�,
         Result => Result_�,
         Req => Request_�);
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags
           (0));
      if not PolyORB.Any.Is_Empty
        (Request_�.Exception_Info)
      then
         Result_�.Argument :=
           Request_�.Exception_Info;
         PolyORB.Requests.Destroy_Request
           (Request_�);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any
           (Result_�.Argument);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request_�);
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any
           (Result_�.Argument));
   end echoULong8;

   echoULong9_Arg_Name_U_arg : PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("arg");

   echoULong9_Operation_Name_� : constant Standard.String :=
     "echoULong9";

   echoULong9_Result_Name_� : CORBA.String :=
     CORBA.To_CORBA_String
        ("Result");

   function echoULong9_Result_�

     return PolyORB.Any.NamedValue
   is
      pragma Inline
        (echoULong9_Result_�);
   begin
      return (Name => PolyORB.Types.Identifier
        (echoULong9_Result_Name_�),
      Argument => CORBA.Internals.To_PolyORB_Any
        (CORBA.Get_Empty_Any
           (CORBA.TC_Unsigned_Long)),
      Arg_Modes => 0);
   end echoULong9_Result_�;

   function echoULong9
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Argument_U_arg : CORBA.Any :=
        CORBA.To_Any
           (arg);
      Self_Ref_� : CORBA.Object.Ref :=
        CORBA.Object.Ref
           (Self);
      Request_� : PolyORB.Requests.Request_Access;
      Result_� : PolyORB.Any.NamedValue :=
        echoULong9_Result_�;
   begin
      if CORBA.Object.Is_Nil
        (Self_Ref_�)
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PolyORB.Any.NVList.Create
        (Argument_List_�);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         echoULong9_Arg_Name_U_arg,
         CORBA.Internals.To_PolyORB_Any
           (Argument_U_arg),
         PolyORB.Any.ARG_IN);
      PolyORB.Requests.Create_Request
        (Target => CORBA.Object.To_PolyORB_Ref
           (CORBA.Object.Ref
              (Self)),
         Operation => echoULong9_Operation_Name_�,
         Arg_List => Argument_List_�,
         Result => Result_�,
         Req => Request_�);
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags
           (0));
      if not PolyORB.Any.Is_Empty
        (Request_�.Exception_Info)
      then
         Result_�.Argument :=
           Request_�.Exception_Info;
         PolyORB.Requests.Destroy_Request
           (Request_�);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any
           (Result_�.Argument);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request_�);
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any
           (Result_�.Argument));
   end echoULong9;

   echoULong10_Arg_Name_U_arg : PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("arg");

   echoULong10_Operation_Name_� : constant Standard.String :=
     "echoULong10";

   echoULong10_Result_Name_� : CORBA.String :=
     CORBA.To_CORBA_String
        ("Result");

   function echoULong10_Result_�

     return PolyORB.Any.NamedValue
   is
      pragma Inline
        (echoULong10_Result_�);
   begin
      return (Name => PolyORB.Types.Identifier
        (echoULong10_Result_Name_�),
      Argument => CORBA.Internals.To_PolyORB_Any
        (CORBA.Get_Empty_Any
           (CORBA.TC_Unsigned_Long)),
      Arg_Modes => 0);
   end echoULong10_Result_�;

   function echoULong10
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Argument_U_arg : CORBA.Any :=
        CORBA.To_Any
           (arg);
      Self_Ref_� : CORBA.Object.Ref :=
        CORBA.Object.Ref
           (Self);
      Request_� : PolyORB.Requests.Request_Access;
      Result_� : PolyORB.Any.NamedValue :=
        echoULong10_Result_�;
   begin
      if CORBA.Object.Is_Nil
        (Self_Ref_�)
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PolyORB.Any.NVList.Create
        (Argument_List_�);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         echoULong10_Arg_Name_U_arg,
         CORBA.Internals.To_PolyORB_Any
           (Argument_U_arg),
         PolyORB.Any.ARG_IN);
      PolyORB.Requests.Create_Request
        (Target => CORBA.Object.To_PolyORB_Ref
           (CORBA.Object.Ref
              (Self)),
         Operation => echoULong10_Operation_Name_�,
         Arg_List => Argument_List_�,
         Result => Result_�,
         Req => Request_�);
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags
           (0));
      if not PolyORB.Any.Is_Empty
        (Request_�.Exception_Info)
      then
         Result_�.Argument :=
           Request_�.Exception_Info;
         PolyORB.Requests.Destroy_Request
           (Request_�);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any
           (Result_�.Argument);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request_�);
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any
           (Result_�.Argument));
   end echoULong10;

   echoULong11_Arg_Name_U_arg : PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("arg");

   echoULong11_Operation_Name_� : constant Standard.String :=
     "echoULong11";

   echoULong11_Result_Name_� : CORBA.String :=
     CORBA.To_CORBA_String
        ("Result");

   function echoULong11_Result_�

     return PolyORB.Any.NamedValue
   is
      pragma Inline
        (echoULong11_Result_�);
   begin
      return (Name => PolyORB.Types.Identifier
        (echoULong11_Result_Name_�),
      Argument => CORBA.Internals.To_PolyORB_Any
        (CORBA.Get_Empty_Any
           (CORBA.TC_Unsigned_Long)),
      Arg_Modes => 0);
   end echoULong11_Result_�;

   function echoULong11
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Argument_U_arg : CORBA.Any :=
        CORBA.To_Any
           (arg);
      Self_Ref_� : CORBA.Object.Ref :=
        CORBA.Object.Ref
           (Self);
      Request_� : PolyORB.Requests.Request_Access;
      Result_� : PolyORB.Any.NamedValue :=
        echoULong11_Result_�;
   begin
      if CORBA.Object.Is_Nil
        (Self_Ref_�)
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PolyORB.Any.NVList.Create
        (Argument_List_�);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         echoULong11_Arg_Name_U_arg,
         CORBA.Internals.To_PolyORB_Any
           (Argument_U_arg),
         PolyORB.Any.ARG_IN);
      PolyORB.Requests.Create_Request
        (Target => CORBA.Object.To_PolyORB_Ref
           (CORBA.Object.Ref
              (Self)),
         Operation => echoULong11_Operation_Name_�,
         Arg_List => Argument_List_�,
         Result => Result_�,
         Req => Request_�);
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags
           (0));
      if not PolyORB.Any.Is_Empty
        (Request_�.Exception_Info)
      then
         Result_�.Argument :=
           Request_�.Exception_Info;
         PolyORB.Requests.Destroy_Request
           (Request_�);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any
           (Result_�.Argument);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request_�);
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any
           (Result_�.Argument));
   end echoULong11;

   echoULong12_Arg_Name_U_arg : PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("arg");

   echoULong12_Operation_Name_� : constant Standard.String :=
     "echoULong12";

   echoULong12_Result_Name_� : CORBA.String :=
     CORBA.To_CORBA_String
        ("Result");

   function echoULong12_Result_�

     return PolyORB.Any.NamedValue
   is
      pragma Inline
        (echoULong12_Result_�);
   begin
      return (Name => PolyORB.Types.Identifier
        (echoULong12_Result_Name_�),
      Argument => CORBA.Internals.To_PolyORB_Any
        (CORBA.Get_Empty_Any
           (CORBA.TC_Unsigned_Long)),
      Arg_Modes => 0);
   end echoULong12_Result_�;

   function echoULong12
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Argument_U_arg : CORBA.Any :=
        CORBA.To_Any
           (arg);
      Self_Ref_� : CORBA.Object.Ref :=
        CORBA.Object.Ref
           (Self);
      Request_� : PolyORB.Requests.Request_Access;
      Result_� : PolyORB.Any.NamedValue :=
        echoULong12_Result_�;
   begin
      if CORBA.Object.Is_Nil
        (Self_Ref_�)
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PolyORB.Any.NVList.Create
        (Argument_List_�);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         echoULong12_Arg_Name_U_arg,
         CORBA.Internals.To_PolyORB_Any
           (Argument_U_arg),
         PolyORB.Any.ARG_IN);
      PolyORB.Requests.Create_Request
        (Target => CORBA.Object.To_PolyORB_Ref
           (CORBA.Object.Ref
              (Self)),
         Operation => echoULong12_Operation_Name_�,
         Arg_List => Argument_List_�,
         Result => Result_�,
         Req => Request_�);
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags
           (0));
      if not PolyORB.Any.Is_Empty
        (Request_�.Exception_Info)
      then
         Result_�.Argument :=
           Request_�.Exception_Info;
         PolyORB.Requests.Destroy_Request
           (Request_�);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any
           (Result_�.Argument);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request_�);
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any
           (Result_�.Argument));
   end echoULong12;

   echoULong13_Arg_Name_U_arg : PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("arg");

   echoULong13_Operation_Name_� : constant Standard.String :=
     "echoULong13";

   echoULong13_Result_Name_� : CORBA.String :=
     CORBA.To_CORBA_String
        ("Result");

   function echoULong13_Result_�

     return PolyORB.Any.NamedValue
   is
      pragma Inline
        (echoULong13_Result_�);
   begin
      return (Name => PolyORB.Types.Identifier
        (echoULong13_Result_Name_�),
      Argument => CORBA.Internals.To_PolyORB_Any
        (CORBA.Get_Empty_Any
           (CORBA.TC_Unsigned_Long)),
      Arg_Modes => 0);
   end echoULong13_Result_�;

   function echoULong13
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Argument_U_arg : CORBA.Any :=
        CORBA.To_Any
           (arg);
      Self_Ref_� : CORBA.Object.Ref :=
        CORBA.Object.Ref
           (Self);
      Request_� : PolyORB.Requests.Request_Access;
      Result_� : PolyORB.Any.NamedValue :=
        echoULong13_Result_�;
   begin
      if CORBA.Object.Is_Nil
        (Self_Ref_�)
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PolyORB.Any.NVList.Create
        (Argument_List_�);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         echoULong13_Arg_Name_U_arg,
         CORBA.Internals.To_PolyORB_Any
           (Argument_U_arg),
         PolyORB.Any.ARG_IN);
      PolyORB.Requests.Create_Request
        (Target => CORBA.Object.To_PolyORB_Ref
           (CORBA.Object.Ref
              (Self)),
         Operation => echoULong13_Operation_Name_�,
         Arg_List => Argument_List_�,
         Result => Result_�,
         Req => Request_�);
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags
           (0));
      if not PolyORB.Any.Is_Empty
        (Request_�.Exception_Info)
      then
         Result_�.Argument :=
           Request_�.Exception_Info;
         PolyORB.Requests.Destroy_Request
           (Request_�);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any
           (Result_�.Argument);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request_�);
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any
           (Result_�.Argument));
   end echoULong13;

   echoULong14_Arg_Name_U_arg : PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("arg");

   echoULong14_Operation_Name_� : constant Standard.String :=
     "echoULong14";

   echoULong14_Result_Name_� : CORBA.String :=
     CORBA.To_CORBA_String
        ("Result");

   function echoULong14_Result_�

     return PolyORB.Any.NamedValue
   is
      pragma Inline
        (echoULong14_Result_�);
   begin
      return (Name => PolyORB.Types.Identifier
        (echoULong14_Result_Name_�),
      Argument => CORBA.Internals.To_PolyORB_Any
        (CORBA.Get_Empty_Any
           (CORBA.TC_Unsigned_Long)),
      Arg_Modes => 0);
   end echoULong14_Result_�;

   function echoULong14
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Argument_U_arg : CORBA.Any :=
        CORBA.To_Any
           (arg);
      Self_Ref_� : CORBA.Object.Ref :=
        CORBA.Object.Ref
           (Self);
      Request_� : PolyORB.Requests.Request_Access;
      Result_� : PolyORB.Any.NamedValue :=
        echoULong14_Result_�;
   begin
      if CORBA.Object.Is_Nil
        (Self_Ref_�)
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PolyORB.Any.NVList.Create
        (Argument_List_�);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         echoULong14_Arg_Name_U_arg,
         CORBA.Internals.To_PolyORB_Any
           (Argument_U_arg),
         PolyORB.Any.ARG_IN);
      PolyORB.Requests.Create_Request
        (Target => CORBA.Object.To_PolyORB_Ref
           (CORBA.Object.Ref
              (Self)),
         Operation => echoULong14_Operation_Name_�,
         Arg_List => Argument_List_�,
         Result => Result_�,
         Req => Request_�);
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags
           (0));
      if not PolyORB.Any.Is_Empty
        (Request_�.Exception_Info)
      then
         Result_�.Argument :=
           Request_�.Exception_Info;
         PolyORB.Requests.Destroy_Request
           (Request_�);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any
           (Result_�.Argument);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request_�);
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any
           (Result_�.Argument));
   end echoULong14;

   echoULong15_Arg_Name_U_arg : PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("arg");

   echoULong15_Operation_Name_� : constant Standard.String :=
     "echoULong15";

   echoULong15_Result_Name_� : CORBA.String :=
     CORBA.To_CORBA_String
        ("Result");

   function echoULong15_Result_�

     return PolyORB.Any.NamedValue
   is
      pragma Inline
        (echoULong15_Result_�);
   begin
      return (Name => PolyORB.Types.Identifier
        (echoULong15_Result_Name_�),
      Argument => CORBA.Internals.To_PolyORB_Any
        (CORBA.Get_Empty_Any
           (CORBA.TC_Unsigned_Long)),
      Arg_Modes => 0);
   end echoULong15_Result_�;

   function echoULong15
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Argument_U_arg : CORBA.Any :=
        CORBA.To_Any
           (arg);
      Self_Ref_� : CORBA.Object.Ref :=
        CORBA.Object.Ref
           (Self);
      Request_� : PolyORB.Requests.Request_Access;
      Result_� : PolyORB.Any.NamedValue :=
        echoULong15_Result_�;
   begin
      if CORBA.Object.Is_Nil
        (Self_Ref_�)
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PolyORB.Any.NVList.Create
        (Argument_List_�);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         echoULong15_Arg_Name_U_arg,
         CORBA.Internals.To_PolyORB_Any
           (Argument_U_arg),
         PolyORB.Any.ARG_IN);
      PolyORB.Requests.Create_Request
        (Target => CORBA.Object.To_PolyORB_Ref
           (CORBA.Object.Ref
              (Self)),
         Operation => echoULong15_Operation_Name_�,
         Arg_List => Argument_List_�,
         Result => Result_�,
         Req => Request_�);
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags
           (0));
      if not PolyORB.Any.Is_Empty
        (Request_�.Exception_Info)
      then
         Result_�.Argument :=
           Request_�.Exception_Info;
         PolyORB.Requests.Destroy_Request
           (Request_�);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any
           (Result_�.Argument);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request_�);
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any
           (Result_�.Argument));
   end echoULong15;

   echoULong16_Arg_Name_U_arg : PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("arg");

   echoULong16_Operation_Name_� : constant Standard.String :=
     "echoULong16";

   echoULong16_Result_Name_� : CORBA.String :=
     CORBA.To_CORBA_String
        ("Result");

   function echoULong16_Result_�

     return PolyORB.Any.NamedValue
   is
      pragma Inline
        (echoULong16_Result_�);
   begin
      return (Name => PolyORB.Types.Identifier
        (echoULong16_Result_Name_�),
      Argument => CORBA.Internals.To_PolyORB_Any
        (CORBA.Get_Empty_Any
           (CORBA.TC_Unsigned_Long)),
      Arg_Modes => 0);
   end echoULong16_Result_�;

   function echoULong16
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Argument_U_arg : CORBA.Any :=
        CORBA.To_Any
           (arg);
      Self_Ref_� : CORBA.Object.Ref :=
        CORBA.Object.Ref
           (Self);
      Request_� : PolyORB.Requests.Request_Access;
      Result_� : PolyORB.Any.NamedValue :=
        echoULong16_Result_�;
   begin
      if CORBA.Object.Is_Nil
        (Self_Ref_�)
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PolyORB.Any.NVList.Create
        (Argument_List_�);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         echoULong16_Arg_Name_U_arg,
         CORBA.Internals.To_PolyORB_Any
           (Argument_U_arg),
         PolyORB.Any.ARG_IN);
      PolyORB.Requests.Create_Request
        (Target => CORBA.Object.To_PolyORB_Ref
           (CORBA.Object.Ref
              (Self)),
         Operation => echoULong16_Operation_Name_�,
         Arg_List => Argument_List_�,
         Result => Result_�,
         Req => Request_�);
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags
           (0));
      if not PolyORB.Any.Is_Empty
        (Request_�.Exception_Info)
      then
         Result_�.Argument :=
           Request_�.Exception_Info;
         PolyORB.Requests.Destroy_Request
           (Request_�);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any
           (Result_�.Argument);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request_�);
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any
           (Result_�.Argument));
   end echoULong16;

   echoULong17_Arg_Name_U_arg : PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("arg");

   echoULong17_Operation_Name_� : constant Standard.String :=
     "echoULong17";

   echoULong17_Result_Name_� : CORBA.String :=
     CORBA.To_CORBA_String
        ("Result");

   function echoULong17_Result_�

     return PolyORB.Any.NamedValue
   is
      pragma Inline
        (echoULong17_Result_�);
   begin
      return (Name => PolyORB.Types.Identifier
        (echoULong17_Result_Name_�),
      Argument => CORBA.Internals.To_PolyORB_Any
        (CORBA.Get_Empty_Any
           (CORBA.TC_Unsigned_Long)),
      Arg_Modes => 0);
   end echoULong17_Result_�;

   function echoULong17
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Argument_U_arg : CORBA.Any :=
        CORBA.To_Any
           (arg);
      Self_Ref_� : CORBA.Object.Ref :=
        CORBA.Object.Ref
           (Self);
      Request_� : PolyORB.Requests.Request_Access;
      Result_� : PolyORB.Any.NamedValue :=
        echoULong17_Result_�;
   begin
      if CORBA.Object.Is_Nil
        (Self_Ref_�)
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PolyORB.Any.NVList.Create
        (Argument_List_�);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         echoULong17_Arg_Name_U_arg,
         CORBA.Internals.To_PolyORB_Any
           (Argument_U_arg),
         PolyORB.Any.ARG_IN);
      PolyORB.Requests.Create_Request
        (Target => CORBA.Object.To_PolyORB_Ref
           (CORBA.Object.Ref
              (Self)),
         Operation => echoULong17_Operation_Name_�,
         Arg_List => Argument_List_�,
         Result => Result_�,
         Req => Request_�);
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags
           (0));
      if not PolyORB.Any.Is_Empty
        (Request_�.Exception_Info)
      then
         Result_�.Argument :=
           Request_�.Exception_Info;
         PolyORB.Requests.Destroy_Request
           (Request_�);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any
           (Result_�.Argument);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request_�);
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any
           (Result_�.Argument));
   end echoULong17;

   echoULong18_Arg_Name_U_arg : PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("arg");

   echoULong18_Operation_Name_� : constant Standard.String :=
     "echoULong18";

   echoULong18_Result_Name_� : CORBA.String :=
     CORBA.To_CORBA_String
        ("Result");

   function echoULong18_Result_�

     return PolyORB.Any.NamedValue
   is
      pragma Inline
        (echoULong18_Result_�);
   begin
      return (Name => PolyORB.Types.Identifier
        (echoULong18_Result_Name_�),
      Argument => CORBA.Internals.To_PolyORB_Any
        (CORBA.Get_Empty_Any
           (CORBA.TC_Unsigned_Long)),
      Arg_Modes => 0);
   end echoULong18_Result_�;

   function echoULong18
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Argument_U_arg : CORBA.Any :=
        CORBA.To_Any
           (arg);
      Self_Ref_� : CORBA.Object.Ref :=
        CORBA.Object.Ref
           (Self);
      Request_� : PolyORB.Requests.Request_Access;
      Result_� : PolyORB.Any.NamedValue :=
        echoULong18_Result_�;
   begin
      if CORBA.Object.Is_Nil
        (Self_Ref_�)
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PolyORB.Any.NVList.Create
        (Argument_List_�);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         echoULong18_Arg_Name_U_arg,
         CORBA.Internals.To_PolyORB_Any
           (Argument_U_arg),
         PolyORB.Any.ARG_IN);
      PolyORB.Requests.Create_Request
        (Target => CORBA.Object.To_PolyORB_Ref
           (CORBA.Object.Ref
              (Self)),
         Operation => echoULong18_Operation_Name_�,
         Arg_List => Argument_List_�,
         Result => Result_�,
         Req => Request_�);
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags
           (0));
      if not PolyORB.Any.Is_Empty
        (Request_�.Exception_Info)
      then
         Result_�.Argument :=
           Request_�.Exception_Info;
         PolyORB.Requests.Destroy_Request
           (Request_�);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any
           (Result_�.Argument);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request_�);
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any
           (Result_�.Argument));
   end echoULong18;

   echoULong19_Arg_Name_U_arg : PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("arg");

   echoULong19_Operation_Name_� : constant Standard.String :=
     "echoULong19";

   echoULong19_Result_Name_� : CORBA.String :=
     CORBA.To_CORBA_String
        ("Result");

   function echoULong19_Result_�

     return PolyORB.Any.NamedValue
   is
      pragma Inline
        (echoULong19_Result_�);
   begin
      return (Name => PolyORB.Types.Identifier
        (echoULong19_Result_Name_�),
      Argument => CORBA.Internals.To_PolyORB_Any
        (CORBA.Get_Empty_Any
           (CORBA.TC_Unsigned_Long)),
      Arg_Modes => 0);
   end echoULong19_Result_�;

   function echoULong19
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Argument_U_arg : CORBA.Any :=
        CORBA.To_Any
           (arg);
      Self_Ref_� : CORBA.Object.Ref :=
        CORBA.Object.Ref
           (Self);
      Request_� : PolyORB.Requests.Request_Access;
      Result_� : PolyORB.Any.NamedValue :=
        echoULong19_Result_�;
   begin
      if CORBA.Object.Is_Nil
        (Self_Ref_�)
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PolyORB.Any.NVList.Create
        (Argument_List_�);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         echoULong19_Arg_Name_U_arg,
         CORBA.Internals.To_PolyORB_Any
           (Argument_U_arg),
         PolyORB.Any.ARG_IN);
      PolyORB.Requests.Create_Request
        (Target => CORBA.Object.To_PolyORB_Ref
           (CORBA.Object.Ref
              (Self)),
         Operation => echoULong19_Operation_Name_�,
         Arg_List => Argument_List_�,
         Result => Result_�,
         Req => Request_�);
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags
           (0));
      if not PolyORB.Any.Is_Empty
        (Request_�.Exception_Info)
      then
         Result_�.Argument :=
           Request_�.Exception_Info;
         PolyORB.Requests.Destroy_Request
           (Request_�);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any
           (Result_�.Argument);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request_�);
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any
           (Result_�.Argument));
   end echoULong19;

   echoULong20_Arg_Name_U_arg : PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("arg");

   echoULong20_Operation_Name_� : constant Standard.String :=
     "echoULong20";

   echoULong20_Result_Name_� : CORBA.String :=
     CORBA.To_CORBA_String
        ("Result");

   function echoULong20_Result_�

     return PolyORB.Any.NamedValue
   is
      pragma Inline
        (echoULong20_Result_�);
   begin
      return (Name => PolyORB.Types.Identifier
        (echoULong20_Result_Name_�),
      Argument => CORBA.Internals.To_PolyORB_Any
        (CORBA.Get_Empty_Any
           (CORBA.TC_Unsigned_Long)),
      Arg_Modes => 0);
   end echoULong20_Result_�;

   function echoULong20
     (Self : in Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Argument_List_� : PolyORB.Any.NVList.Ref;
      Argument_U_arg : CORBA.Any :=
        CORBA.To_Any
           (arg);
      Self_Ref_� : CORBA.Object.Ref :=
        CORBA.Object.Ref
           (Self);
      Request_� : PolyORB.Requests.Request_Access;
      Result_� : PolyORB.Any.NamedValue :=
        echoULong20_Result_�;
   begin
      if CORBA.Object.Is_Nil
        (Self_Ref_�)
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;
      PolyORB.Any.NVList.Create
        (Argument_List_�);
      PolyORB.Any.NVList.Add_Item
        (Argument_List_�,
         echoULong20_Arg_Name_U_arg,
         CORBA.Internals.To_PolyORB_Any
           (Argument_U_arg),
         PolyORB.Any.ARG_IN);
      PolyORB.Requests.Create_Request
        (Target => CORBA.Object.To_PolyORB_Ref
           (CORBA.Object.Ref
              (Self)),
         Operation => echoULong20_Operation_Name_�,
         Arg_List => Argument_List_�,
         Result => Result_�,
         Req => Request_�);
      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�,
         PolyORB.Requests.Flags
           (0));
      if not PolyORB.Any.Is_Empty
        (Request_�.Exception_Info)
      then
         Result_�.Argument :=
           Request_�.Exception_Info;
         PolyORB.Requests.Destroy_Request
           (Request_�);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any
           (Result_�.Argument);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request_�);
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any
           (Result_�.Argument));
   end echoULong20;

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
