-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.act-europe.fr/polyorb/)
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
-------------------------------------------------
pragma Style_Checks (Off);

with PolyORB.Types;
with PolyORB.Requests;
with PolyORB.Any.NVList;
with PolyORB.CORBA_P.Interceptors_Hooks;
with PolyORB.CORBA_P.Exceptions;
with CORBA.Object;
with CORBA;
 use CORBA;
pragma Elaborate_All (CORBA);

package body Harness is

   function echoULong1
     (Self : Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Arg_Name_Ü_arg : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("arg");
      Argument_Ü_arg : CORBA.Any
        := CORBA.To_Any
        (arg);

      Operation_Name_Ü : constant Standard.String
        := "echoULong1";
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
         Arg_Name_Ü_arg,
         CORBA.Internals.To_PolyORB_Any (Argument_Ü_arg),
         PolyORB.Any.ARG_IN);
      --  Set result type (maybe void)
      Result_Ü
        := (Name => PolyORB.Types.Identifier (Result_Name_Ü),
            Argument => CORBA.Internals.To_PolyORB_Any 
        (Get_Empty_Any (CORBA.TC_Unsigned_Long)),
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
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any (Result_Ü.Argument));
   end echoULong1;

   function echoULong2
     (Self : Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Arg_Name_Ü_arg : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("arg");
      Argument_Ü_arg : CORBA.Any
        := CORBA.To_Any
        (arg);

      Operation_Name_Ü : constant Standard.String
        := "echoULong2";
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
         Arg_Name_Ü_arg,
         CORBA.Internals.To_PolyORB_Any (Argument_Ü_arg),
         PolyORB.Any.ARG_IN);
      --  Set result type (maybe void)
      Result_Ü
        := (Name => PolyORB.Types.Identifier (Result_Name_Ü),
            Argument => CORBA.Internals.To_PolyORB_Any 
        (Get_Empty_Any (CORBA.TC_Unsigned_Long)),
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
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any (Result_Ü.Argument));
   end echoULong2;

   function echoULong3
     (Self : Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Arg_Name_Ü_arg : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("arg");
      Argument_Ü_arg : CORBA.Any
        := CORBA.To_Any
        (arg);

      Operation_Name_Ü : constant Standard.String
        := "echoULong3";
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
         Arg_Name_Ü_arg,
         CORBA.Internals.To_PolyORB_Any (Argument_Ü_arg),
         PolyORB.Any.ARG_IN);
      --  Set result type (maybe void)
      Result_Ü
        := (Name => PolyORB.Types.Identifier (Result_Name_Ü),
            Argument => CORBA.Internals.To_PolyORB_Any 
        (Get_Empty_Any (CORBA.TC_Unsigned_Long)),
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
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any (Result_Ü.Argument));
   end echoULong3;

   function echoULong4
     (Self : Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Arg_Name_Ü_arg : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("arg");
      Argument_Ü_arg : CORBA.Any
        := CORBA.To_Any
        (arg);

      Operation_Name_Ü : constant Standard.String
        := "echoULong4";
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
         Arg_Name_Ü_arg,
         CORBA.Internals.To_PolyORB_Any (Argument_Ü_arg),
         PolyORB.Any.ARG_IN);
      --  Set result type (maybe void)
      Result_Ü
        := (Name => PolyORB.Types.Identifier (Result_Name_Ü),
            Argument => CORBA.Internals.To_PolyORB_Any 
        (Get_Empty_Any (CORBA.TC_Unsigned_Long)),
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
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any (Result_Ü.Argument));
   end echoULong4;

   function echoULong5
     (Self : Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Arg_Name_Ü_arg : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("arg");
      Argument_Ü_arg : CORBA.Any
        := CORBA.To_Any
        (arg);

      Operation_Name_Ü : constant Standard.String
        := "echoULong5";
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
         Arg_Name_Ü_arg,
         CORBA.Internals.To_PolyORB_Any (Argument_Ü_arg),
         PolyORB.Any.ARG_IN);
      --  Set result type (maybe void)
      Result_Ü
        := (Name => PolyORB.Types.Identifier (Result_Name_Ü),
            Argument => CORBA.Internals.To_PolyORB_Any 
        (Get_Empty_Any (CORBA.TC_Unsigned_Long)),
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
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any (Result_Ü.Argument));
   end echoULong5;

   function echoULong6
     (Self : Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Arg_Name_Ü_arg : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("arg");
      Argument_Ü_arg : CORBA.Any
        := CORBA.To_Any
        (arg);

      Operation_Name_Ü : constant Standard.String
        := "echoULong6";
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
         Arg_Name_Ü_arg,
         CORBA.Internals.To_PolyORB_Any (Argument_Ü_arg),
         PolyORB.Any.ARG_IN);
      --  Set result type (maybe void)
      Result_Ü
        := (Name => PolyORB.Types.Identifier (Result_Name_Ü),
            Argument => CORBA.Internals.To_PolyORB_Any 
        (Get_Empty_Any (CORBA.TC_Unsigned_Long)),
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
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any (Result_Ü.Argument));
   end echoULong6;

   function echoULong7
     (Self : Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Arg_Name_Ü_arg : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("arg");
      Argument_Ü_arg : CORBA.Any
        := CORBA.To_Any
        (arg);

      Operation_Name_Ü : constant Standard.String
        := "echoULong7";
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
         Arg_Name_Ü_arg,
         CORBA.Internals.To_PolyORB_Any (Argument_Ü_arg),
         PolyORB.Any.ARG_IN);
      --  Set result type (maybe void)
      Result_Ü
        := (Name => PolyORB.Types.Identifier (Result_Name_Ü),
            Argument => CORBA.Internals.To_PolyORB_Any 
        (Get_Empty_Any (CORBA.TC_Unsigned_Long)),
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
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any (Result_Ü.Argument));
   end echoULong7;

   function echoULong8
     (Self : Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Arg_Name_Ü_arg : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("arg");
      Argument_Ü_arg : CORBA.Any
        := CORBA.To_Any
        (arg);

      Operation_Name_Ü : constant Standard.String
        := "echoULong8";
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
         Arg_Name_Ü_arg,
         CORBA.Internals.To_PolyORB_Any (Argument_Ü_arg),
         PolyORB.Any.ARG_IN);
      --  Set result type (maybe void)
      Result_Ü
        := (Name => PolyORB.Types.Identifier (Result_Name_Ü),
            Argument => CORBA.Internals.To_PolyORB_Any 
        (Get_Empty_Any (CORBA.TC_Unsigned_Long)),
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
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any (Result_Ü.Argument));
   end echoULong8;

   function echoULong9
     (Self : Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Arg_Name_Ü_arg : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("arg");
      Argument_Ü_arg : CORBA.Any
        := CORBA.To_Any
        (arg);

      Operation_Name_Ü : constant Standard.String
        := "echoULong9";
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
         Arg_Name_Ü_arg,
         CORBA.Internals.To_PolyORB_Any (Argument_Ü_arg),
         PolyORB.Any.ARG_IN);
      --  Set result type (maybe void)
      Result_Ü
        := (Name => PolyORB.Types.Identifier (Result_Name_Ü),
            Argument => CORBA.Internals.To_PolyORB_Any 
        (Get_Empty_Any (CORBA.TC_Unsigned_Long)),
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
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any (Result_Ü.Argument));
   end echoULong9;

   function echoULong10
     (Self : Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Arg_Name_Ü_arg : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("arg");
      Argument_Ü_arg : CORBA.Any
        := CORBA.To_Any
        (arg);

      Operation_Name_Ü : constant Standard.String
        := "echoULong10";
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
         Arg_Name_Ü_arg,
         CORBA.Internals.To_PolyORB_Any (Argument_Ü_arg),
         PolyORB.Any.ARG_IN);
      --  Set result type (maybe void)
      Result_Ü
        := (Name => PolyORB.Types.Identifier (Result_Name_Ü),
            Argument => CORBA.Internals.To_PolyORB_Any 
        (Get_Empty_Any (CORBA.TC_Unsigned_Long)),
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
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any (Result_Ü.Argument));
   end echoULong10;

   function echoULong11
     (Self : Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Arg_Name_Ü_arg : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("arg");
      Argument_Ü_arg : CORBA.Any
        := CORBA.To_Any
        (arg);

      Operation_Name_Ü : constant Standard.String
        := "echoULong11";
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
         Arg_Name_Ü_arg,
         CORBA.Internals.To_PolyORB_Any (Argument_Ü_arg),
         PolyORB.Any.ARG_IN);
      --  Set result type (maybe void)
      Result_Ü
        := (Name => PolyORB.Types.Identifier (Result_Name_Ü),
            Argument => CORBA.Internals.To_PolyORB_Any 
        (Get_Empty_Any (CORBA.TC_Unsigned_Long)),
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
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any (Result_Ü.Argument));
   end echoULong11;

   function echoULong12
     (Self : Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Arg_Name_Ü_arg : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("arg");
      Argument_Ü_arg : CORBA.Any
        := CORBA.To_Any
        (arg);

      Operation_Name_Ü : constant Standard.String
        := "echoULong12";
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
         Arg_Name_Ü_arg,
         CORBA.Internals.To_PolyORB_Any (Argument_Ü_arg),
         PolyORB.Any.ARG_IN);
      --  Set result type (maybe void)
      Result_Ü
        := (Name => PolyORB.Types.Identifier (Result_Name_Ü),
            Argument => CORBA.Internals.To_PolyORB_Any 
        (Get_Empty_Any (CORBA.TC_Unsigned_Long)),
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
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any (Result_Ü.Argument));
   end echoULong12;

   function echoULong13
     (Self : Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Arg_Name_Ü_arg : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("arg");
      Argument_Ü_arg : CORBA.Any
        := CORBA.To_Any
        (arg);

      Operation_Name_Ü : constant Standard.String
        := "echoULong13";
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
         Arg_Name_Ü_arg,
         CORBA.Internals.To_PolyORB_Any (Argument_Ü_arg),
         PolyORB.Any.ARG_IN);
      --  Set result type (maybe void)
      Result_Ü
        := (Name => PolyORB.Types.Identifier (Result_Name_Ü),
            Argument => CORBA.Internals.To_PolyORB_Any 
        (Get_Empty_Any (CORBA.TC_Unsigned_Long)),
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
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any (Result_Ü.Argument));
   end echoULong13;

   function echoULong14
     (Self : Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Arg_Name_Ü_arg : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("arg");
      Argument_Ü_arg : CORBA.Any
        := CORBA.To_Any
        (arg);

      Operation_Name_Ü : constant Standard.String
        := "echoULong14";
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
         Arg_Name_Ü_arg,
         CORBA.Internals.To_PolyORB_Any (Argument_Ü_arg),
         PolyORB.Any.ARG_IN);
      --  Set result type (maybe void)
      Result_Ü
        := (Name => PolyORB.Types.Identifier (Result_Name_Ü),
            Argument => CORBA.Internals.To_PolyORB_Any 
        (Get_Empty_Any (CORBA.TC_Unsigned_Long)),
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
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any (Result_Ü.Argument));
   end echoULong14;

   function echoULong15
     (Self : Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Arg_Name_Ü_arg : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("arg");
      Argument_Ü_arg : CORBA.Any
        := CORBA.To_Any
        (arg);

      Operation_Name_Ü : constant Standard.String
        := "echoULong15";
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
         Arg_Name_Ü_arg,
         CORBA.Internals.To_PolyORB_Any (Argument_Ü_arg),
         PolyORB.Any.ARG_IN);
      --  Set result type (maybe void)
      Result_Ü
        := (Name => PolyORB.Types.Identifier (Result_Name_Ü),
            Argument => CORBA.Internals.To_PolyORB_Any 
        (Get_Empty_Any (CORBA.TC_Unsigned_Long)),
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
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any (Result_Ü.Argument));
   end echoULong15;

   function echoULong16
     (Self : Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Arg_Name_Ü_arg : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("arg");
      Argument_Ü_arg : CORBA.Any
        := CORBA.To_Any
        (arg);

      Operation_Name_Ü : constant Standard.String
        := "echoULong16";
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
         Arg_Name_Ü_arg,
         CORBA.Internals.To_PolyORB_Any (Argument_Ü_arg),
         PolyORB.Any.ARG_IN);
      --  Set result type (maybe void)
      Result_Ü
        := (Name => PolyORB.Types.Identifier (Result_Name_Ü),
            Argument => CORBA.Internals.To_PolyORB_Any 
        (Get_Empty_Any (CORBA.TC_Unsigned_Long)),
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
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any (Result_Ü.Argument));
   end echoULong16;

   function echoULong17
     (Self : Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Arg_Name_Ü_arg : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("arg");
      Argument_Ü_arg : CORBA.Any
        := CORBA.To_Any
        (arg);

      Operation_Name_Ü : constant Standard.String
        := "echoULong17";
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
         Arg_Name_Ü_arg,
         CORBA.Internals.To_PolyORB_Any (Argument_Ü_arg),
         PolyORB.Any.ARG_IN);
      --  Set result type (maybe void)
      Result_Ü
        := (Name => PolyORB.Types.Identifier (Result_Name_Ü),
            Argument => CORBA.Internals.To_PolyORB_Any 
        (Get_Empty_Any (CORBA.TC_Unsigned_Long)),
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
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any (Result_Ü.Argument));
   end echoULong17;

   function echoULong18
     (Self : Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Arg_Name_Ü_arg : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("arg");
      Argument_Ü_arg : CORBA.Any
        := CORBA.To_Any
        (arg);

      Operation_Name_Ü : constant Standard.String
        := "echoULong18";
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
         Arg_Name_Ü_arg,
         CORBA.Internals.To_PolyORB_Any (Argument_Ü_arg),
         PolyORB.Any.ARG_IN);
      --  Set result type (maybe void)
      Result_Ü
        := (Name => PolyORB.Types.Identifier (Result_Name_Ü),
            Argument => CORBA.Internals.To_PolyORB_Any 
        (Get_Empty_Any (CORBA.TC_Unsigned_Long)),
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
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any (Result_Ü.Argument));
   end echoULong18;

   function echoULong19
     (Self : Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Arg_Name_Ü_arg : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("arg");
      Argument_Ü_arg : CORBA.Any
        := CORBA.To_Any
        (arg);

      Operation_Name_Ü : constant Standard.String
        := "echoULong19";
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
         Arg_Name_Ü_arg,
         CORBA.Internals.To_PolyORB_Any (Argument_Ü_arg),
         PolyORB.Any.ARG_IN);
      --  Set result type (maybe void)
      Result_Ü
        := (Name => PolyORB.Types.Identifier (Result_Name_Ü),
            Argument => CORBA.Internals.To_PolyORB_Any 
        (Get_Empty_Any (CORBA.TC_Unsigned_Long)),
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
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any (Result_Ü.Argument));
   end echoULong19;

   function echoULong20
     (Self : Ref;
      arg : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      Arg_Name_Ü_arg : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("arg");
      Argument_Ü_arg : CORBA.Any
        := CORBA.To_Any
        (arg);

      Operation_Name_Ü : constant Standard.String
        := "echoULong20";
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
         Arg_Name_Ü_arg,
         CORBA.Internals.To_PolyORB_Any (Argument_Ü_arg),
         PolyORB.Any.ARG_IN);
      --  Set result type (maybe void)
      Result_Ü
        := (Name => PolyORB.Types.Identifier (Result_Name_Ü),
            Argument => CORBA.Internals.To_PolyORB_Any 
        (Get_Empty_Any (CORBA.TC_Unsigned_Long)),
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
      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any (Result_Ü.Argument));
   end echoULong20;

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
         Harness.Repository_Id)
        or else CORBA.Is_Equivalent
          (Logical_Type_Id,
           "IDL:omg.org/CORBA/Object:1.0")
        or else False;

   end Is_A;

end Harness;
