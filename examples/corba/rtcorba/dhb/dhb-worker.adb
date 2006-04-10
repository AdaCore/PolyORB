-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.adacore.com/polyorb/)
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
-------------------------------------------------
pragma Style_Checks ("NM32766");

with RTCORBA.Helper;
with DHB.Worker.Helper;
with DHB.Helper;
with PolyORB.Requests;
with PolyORB.Any.NVList;
with PolyORB.CORBA_P.Interceptors_Hooks;
with PolyORB.CORBA_P.Exceptions;
with CORBA;
 use CORBA;
pragma Elaborate_All (CORBA);
with PolyORB.Types;

package body DHB.Worker is

   Result_Name_� : constant PolyORB.Types.Identifier
     := PolyORB.Types.To_PolyORB_String ("Result");

   Do_Some_Work_Arg_Name_�_Kilo_Whetstone : PolyORB.Types.Identifier
     := PolyORB.Types.To_PolyORB_String ("Kilo_Whetstone");

   procedure Do_Some_Work
     (Self : Ref;
      Kilo_Whetstone : in DHB.KWIPS)
   is
      --  Prepare in arguments

      Arg_Any_�_Kilo_Whetstone : CORBA.Any
        := DHB.Helper.To_Any
        (Kilo_Whetstone);
      Self_Ref_� : CORBA.Object.Ref
        := CORBA.Object.Ref (Self);

      Request_� : PolyORB.Requests.Request_Access;
      Arg_List_� : PolyORB.Any.NVList.Ref;
      Result_� : PolyORB.Any.NamedValue;
   begin

      if CORBA.Object.Is_Nil (Self_Ref_�) then
         CORBA.Raise_Inv_Objref (Default_Sys_Member);
      end if;

      --  Create argument list
      PolyORB.Any.NVList.Create
        (Arg_List_�);
      PolyORB.Any.NVList.Add_Item
        (Arg_List_�,
         Do_Some_Work_Arg_Name_�_Kilo_Whetstone,
         CORBA.Internals.To_PolyORB_Any (Arg_Any_�_Kilo_Whetstone),
         PolyORB.Any.ARG_IN);

      --  Set result type (maybe void)

      Result_� :=
       (Name     => Result_Name_�,
        Argument => CORBA.Internals.To_PolyORB_Any (
         CORBA.Internals.Get_Empty_Any (CORBA.TC_Void)),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => CORBA.Object.Internals.To_PolyORB_Ref
           (CORBA.Object.Ref (Self)),
         Operation => "Do_Some_Work",
         Arg_List  => Arg_List_�,
         Result    => Result_�,
         Req       => Request_�);

      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�, PolyORB.Requests.Flags (0));
      if not PolyORB.Any.Is_Empty (Request_�.Exception_Info) then
         Result_�.Argument := Request_�.Exception_Info;
         PolyORB.Requests.Destroy_Request (Request_�);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any (Result_�.Argument);

         --  Not reached

      end if;
      PolyORB.Requests.Destroy_Request (Request_�);

      --  Request has been synchronously invoked.
   end Do_Some_Work;

   Do_Some_Work_With_Payload_Arg_Name_�_Kilo_Whetstone : PolyORB.Types.Identifier
     := PolyORB.Types.To_PolyORB_String ("Kilo_Whetstone");

   Do_Some_Work_With_Payload_Arg_Name_�_Payload : PolyORB.Types.Identifier
     := PolyORB.Types.To_PolyORB_String ("Payload");

   procedure Do_Some_Work_With_Payload
     (Self : Ref;
      Kilo_Whetstone : in DHB.KWIPS;
      Payload : in DHB.Worker.U_sequence)
   is
      --  Prepare in arguments

      Arg_Any_�_Kilo_Whetstone : CORBA.Any
        := DHB.Helper.To_Any
        (Kilo_Whetstone);Arg_Any_�_Payload : CORBA.Any
        := DHB.Worker.Helper.To_Any
        (Payload);
      Self_Ref_� : CORBA.Object.Ref
        := CORBA.Object.Ref (Self);

      Request_� : PolyORB.Requests.Request_Access;
      Arg_List_� : PolyORB.Any.NVList.Ref;
      Result_� : PolyORB.Any.NamedValue;
   begin

      if CORBA.Object.Is_Nil (Self_Ref_�) then
         CORBA.Raise_Inv_Objref (Default_Sys_Member);
      end if;

      --  Create argument list
      PolyORB.Any.NVList.Create
        (Arg_List_�);
      PolyORB.Any.NVList.Add_Item
        (Arg_List_�,
         Do_Some_Work_With_Payload_Arg_Name_�_Kilo_Whetstone,
         CORBA.Internals.To_PolyORB_Any (Arg_Any_�_Kilo_Whetstone),
         PolyORB.Any.ARG_IN);
      PolyORB.Any.NVList.Add_Item
        (Arg_List_�,
         Do_Some_Work_With_Payload_Arg_Name_�_Payload,
         CORBA.Internals.To_PolyORB_Any (Arg_Any_�_Payload),
         PolyORB.Any.ARG_IN);

      --  Set result type (maybe void)

      Result_� :=
       (Name     => Result_Name_�,
        Argument => CORBA.Internals.To_PolyORB_Any (
         CORBA.Internals.Get_Empty_Any (CORBA.TC_Void)),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => CORBA.Object.Internals.To_PolyORB_Ref
           (CORBA.Object.Ref (Self)),
         Operation => "Do_Some_Work_With_Payload",
         Arg_List  => Arg_List_�,
         Result    => Result_�,
         Req       => Request_�);

      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�, PolyORB.Requests.Flags (0));
      if not PolyORB.Any.Is_Empty (Request_�.Exception_Info) then
         Result_�.Argument := Request_�.Exception_Info;
         PolyORB.Requests.Destroy_Request (Request_�);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any (Result_�.Argument);

         --  Not reached

      end if;
      PolyORB.Requests.Destroy_Request (Request_�);

      --  Request has been synchronously invoked.
   end Do_Some_Work_With_Payload;

   function Get_KWIPS
     (Self : Ref)
     return DHB.KWIPS
   is
      --  Prepare in arguments


      Self_Ref_� : CORBA.Object.Ref
        := CORBA.Object.Ref (Self);

      Request_� : PolyORB.Requests.Request_Access;
      Arg_List_� : PolyORB.Any.NVList.Ref;
      Result_� : PolyORB.Any.NamedValue;
   begin

      if CORBA.Object.Is_Nil (Self_Ref_�) then
         CORBA.Raise_Inv_Objref (Default_Sys_Member);
      end if;

      --  Create argument list
      PolyORB.Any.NVList.Create
        (Arg_List_�);

      --  Set result type (maybe void)

      Result_� :=
       (Name     => Result_Name_�,
        Argument => CORBA.Internals.To_PolyORB_Any (
         CORBA.Internals.Get_Empty_Any (DHB.Helper.TC_KWIPS)),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => CORBA.Object.Internals.To_PolyORB_Ref
           (CORBA.Object.Ref (Self)),
         Operation => "Get_KWIPS",
         Arg_List  => Arg_List_�,
         Result    => Result_�,
         Req       => Request_�);

      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�, PolyORB.Requests.Flags (0));
      if not PolyORB.Any.Is_Empty (Request_�.Exception_Info) then
         Result_�.Argument := Request_�.Exception_Info;
         PolyORB.Requests.Destroy_Request (Request_�);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any (Result_�.Argument);

         --  Not reached

      end if;
      PolyORB.Requests.Destroy_Request (Request_�);

      --  Request has been synchronously invoked.

      --  Retrieve return value

      return DHB.Helper.From_Any
        (CORBA.Internals.To_CORBA_Any (Result_�.Argument));
   end Get_KWIPS;

   function Running_Priority
     (Self : Ref)
     return RTCORBA.Priority
   is
      --  Prepare in arguments


      Self_Ref_� : CORBA.Object.Ref
        := CORBA.Object.Ref (Self);

      Request_� : PolyORB.Requests.Request_Access;
      Arg_List_� : PolyORB.Any.NVList.Ref;
      Result_� : PolyORB.Any.NamedValue;
   begin

      if CORBA.Object.Is_Nil (Self_Ref_�) then
         CORBA.Raise_Inv_Objref (Default_Sys_Member);
      end if;

      --  Create argument list
      PolyORB.Any.NVList.Create
        (Arg_List_�);

      --  Set result type (maybe void)

      Result_� :=
       (Name     => Result_Name_�,
        Argument => CORBA.Internals.To_PolyORB_Any (
         CORBA.Internals.Get_Empty_Any (RTCORBA.Helper.TC_Priority)),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => CORBA.Object.Internals.To_PolyORB_Ref
           (CORBA.Object.Ref (Self)),
         Operation => "Running_Priority",
         Arg_List  => Arg_List_�,
         Result    => Result_�,
         Req       => Request_�);

      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�, PolyORB.Requests.Flags (0));
      if not PolyORB.Any.Is_Empty (Request_�.Exception_Info) then
         Result_�.Argument := Request_�.Exception_Info;
         PolyORB.Requests.Destroy_Request (Request_�);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any (Result_�.Argument);

         --  Not reached

      end if;
      PolyORB.Requests.Destroy_Request (Request_�);

      --  Request has been synchronously invoked.

      --  Retrieve return value

      return RTCORBA.Helper.From_Any
        (CORBA.Internals.To_CORBA_Any (Result_�.Argument));
   end Running_Priority;

   Round_Trip_Arg_Name_�_data : PolyORB.Types.Identifier
     := PolyORB.Types.To_PolyORB_String ("data");

   function Round_Trip
     (Self : Ref;
      data : in CORBA.Unsigned_Long)
     return CORBA.Unsigned_Long
   is
      --  Prepare in arguments

      Arg_Any_�_data : CORBA.Any
        := CORBA.To_Any
        (data);
      Self_Ref_� : CORBA.Object.Ref
        := CORBA.Object.Ref (Self);

      Request_� : PolyORB.Requests.Request_Access;
      Arg_List_� : PolyORB.Any.NVList.Ref;
      Result_� : PolyORB.Any.NamedValue;
   begin

      if CORBA.Object.Is_Nil (Self_Ref_�) then
         CORBA.Raise_Inv_Objref (Default_Sys_Member);
      end if;

      --  Create argument list
      PolyORB.Any.NVList.Create
        (Arg_List_�);
      PolyORB.Any.NVList.Add_Item
        (Arg_List_�,
         Round_Trip_Arg_Name_�_data,
         CORBA.Internals.To_PolyORB_Any (Arg_Any_�_data),
         PolyORB.Any.ARG_IN);

      --  Set result type (maybe void)

      Result_� :=
       (Name     => Result_Name_�,
        Argument => CORBA.Internals.To_PolyORB_Any (
         CORBA.Internals.Get_Empty_Any (CORBA.TC_Unsigned_Long)),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => CORBA.Object.Internals.To_PolyORB_Ref
           (CORBA.Object.Ref (Self)),
         Operation => "Round_Trip",
         Arg_List  => Arg_List_�,
         Result    => Result_�,
         Req       => Request_�);

      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�, PolyORB.Requests.Flags (0));
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
   end Round_Trip;

   Round_Trip_With_Payload_Arg_Name_�_Payload : PolyORB.Types.Identifier
     := PolyORB.Types.To_PolyORB_String ("Payload");

   function Round_Trip_With_Payload
     (Self : Ref;
      Payload : in DHB.Worker.U_sequence)
     return DHB.Worker.U_sequence
   is
      --  Prepare in arguments

      Arg_Any_�_Payload : CORBA.Any
        := DHB.Worker.Helper.To_Any
        (Payload);
      Self_Ref_� : CORBA.Object.Ref
        := CORBA.Object.Ref (Self);

      Request_� : PolyORB.Requests.Request_Access;
      Arg_List_� : PolyORB.Any.NVList.Ref;
      Result_� : PolyORB.Any.NamedValue;
   begin

      if CORBA.Object.Is_Nil (Self_Ref_�) then
         CORBA.Raise_Inv_Objref (Default_Sys_Member);
      end if;

      --  Create argument list
      PolyORB.Any.NVList.Create
        (Arg_List_�);
      PolyORB.Any.NVList.Add_Item
        (Arg_List_�,
         Round_Trip_With_Payload_Arg_Name_�_Payload,
         CORBA.Internals.To_PolyORB_Any (Arg_Any_�_Payload),
         PolyORB.Any.ARG_IN);

      --  Set result type (maybe void)

      Result_� :=
       (Name     => Result_Name_�,
        Argument => CORBA.Internals.To_PolyORB_Any (
         CORBA.Internals.Get_Empty_Any (DHB.Worker.Helper.TC_U_sequence)),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => CORBA.Object.Internals.To_PolyORB_Ref
           (CORBA.Object.Ref (Self)),
         Operation => "Round_Trip_With_Payload",
         Arg_List  => Arg_List_�,
         Result    => Result_�,
         Req       => Request_�);

      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�, PolyORB.Requests.Flags (0));
      if not PolyORB.Any.Is_Empty (Request_�.Exception_Info) then
         Result_�.Argument := Request_�.Exception_Info;
         PolyORB.Requests.Destroy_Request (Request_�);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any (Result_�.Argument);

         --  Not reached

      end if;
      PolyORB.Requests.Destroy_Request (Request_�);

      --  Request has been synchronously invoked.

      --  Retrieve return value

      return DHB.Worker.Helper.From_Any
        (CORBA.Internals.To_CORBA_Any (Result_�.Argument));
   end Round_Trip_With_Payload;

   Ping_Arg_Name_�_data : PolyORB.Types.Identifier
     := PolyORB.Types.To_PolyORB_String ("data");

   procedure Ping
     (Self : Ref;
      data : in CORBA.Unsigned_Long)
   is
      --  Prepare in arguments

      Arg_Any_�_data : CORBA.Any
        := CORBA.To_Any
        (data);
      Self_Ref_� : CORBA.Object.Ref
        := CORBA.Object.Ref (Self);

      Request_� : PolyORB.Requests.Request_Access;
      Arg_List_� : PolyORB.Any.NVList.Ref;
      Result_� : PolyORB.Any.NamedValue;
   begin

      if CORBA.Object.Is_Nil (Self_Ref_�) then
         CORBA.Raise_Inv_Objref (Default_Sys_Member);
      end if;

      --  Create argument list
      PolyORB.Any.NVList.Create
        (Arg_List_�);
      PolyORB.Any.NVList.Add_Item
        (Arg_List_�,
         Ping_Arg_Name_�_data,
         CORBA.Internals.To_PolyORB_Any (Arg_Any_�_data),
         PolyORB.Any.ARG_IN);

      --  Set result type (maybe void)

      Result_� :=
       (Name     => Result_Name_�,
        Argument => CORBA.Internals.To_PolyORB_Any (
         CORBA.Internals.Get_Empty_Any (CORBA.TC_Void)),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => CORBA.Object.Internals.To_PolyORB_Ref
           (CORBA.Object.Ref (Self)),
         Operation => "Ping",
         Arg_List  => Arg_List_�,
         Result    => Result_�,
         Req       => Request_�,
         Req_Flags => PolyORB.Requests.Sync_With_Transport);

      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request_�, PolyORB.Requests.Flags (0));
      if not PolyORB.Any.Is_Empty (Request_�.Exception_Info) then
         Result_�.Argument := Request_�.Exception_Info;
         PolyORB.Requests.Destroy_Request (Request_�);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any (Result_�.Argument);

         --  Not reached

      end if;
      PolyORB.Requests.Destroy_Request (Request_�);
   end Ping;

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
         --  Locally check class membership for this interface

        or else Is_A (Logical_Type_Id)
         --  Fall back to a remote membership check (may involve
         --  an actual request invocation on Self).

           or else CORBA.Object.Is_A
                    (CORBA.Object.Ref (Self), Logical_Type_Id);

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
         DHB.Worker.Repository_Id)
        or else CORBA.Is_Equivalent
          (Logical_Type_Id,
           "IDL:omg.org/CORBA/Object:1.0")
        or else False;

   end Is_A;

end DHB.Worker;
