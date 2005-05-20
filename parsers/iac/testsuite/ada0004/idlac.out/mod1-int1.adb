-------------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://libre.adacore.com/polyorb/)
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
-------------------------------------------------
pragma Style_Checks (Off);

with mod1.Helper;
with mod1.Int1.Helper;
with PolyORB.Types;
with PolyORB.Requests;
with PolyORB.Any.NVList;
with PolyORB.CORBA_P.Interceptors_Hooks;
with PolyORB.CORBA_P.Exceptions;
with CORBA.Object;
with CORBA;
 use CORBA;
pragma Elaborate_All (CORBA);

package body mod1.Int1 is

   function get_Real_Number
     (Self : Ref)
     return mod1.Int1.New_Float
   is
      --  Prepare in arguments


      Operation_Name_Ü : constant Standard.String
        := "_get_Real_Number";
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
        (CORBA.Internals.Get_Empty_Any (mod1.Int1.Helper.TC_New_Float)),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => CORBA.Object.Internals.To_PolyORB_Ref
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
         PolyORB.Requests.Destroy_Request (Request_Ü);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any (Result_Ü.Argument);

         --  Not reached

      end if;
      PolyORB.Requests.Destroy_Request (Request_Ü);

      --  Request has been synchronously invoked.

      --  Retrieve return value

      return mod1.Int1.Helper.From_Any
        (CORBA.Internals.To_CORBA_Any (Result_Ü.Argument));
   end get_Real_Number;

   procedure set_Real_Number
     (Self : Ref;
      To : in mod1.Int1.New_Float)
   is
      --  Prepare in arguments

      Arg_Name_Ü_To : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("To");
      Argument_Ü_To : CORBA.Any
        := mod1.Int1.Helper.To_Any
        (To);
      Operation_Name_Ü : constant Standard.String
        := "_set_Real_Number";
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
        (CORBA.Internals.Get_Empty_Any (CORBA.TC_Void)),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => CORBA.Object.Internals.To_PolyORB_Ref
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
         PolyORB.Requests.Destroy_Request (Request_Ü);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any (Result_Ü.Argument);

         --  Not reached

      end if;
      PolyORB.Requests.Destroy_Request (Request_Ü);

      --  Request has been synchronously invoked.
   end set_Real_Number;

   function get_couleur
     (Self : Ref)
     return mod1.Int1.Color
   is
      --  Prepare in arguments


      Operation_Name_Ü : constant Standard.String
        := "_get_couleur";
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
        (CORBA.Internals.Get_Empty_Any (mod1.Int1.Helper.TC_Color)),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => CORBA.Object.Internals.To_PolyORB_Ref
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
         PolyORB.Requests.Destroy_Request (Request_Ü);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any (Result_Ü.Argument);

         --  Not reached

      end if;
      PolyORB.Requests.Destroy_Request (Request_Ü);

      --  Request has been synchronously invoked.

      --  Retrieve return value

      return mod1.Int1.Helper.From_Any
        (CORBA.Internals.To_CORBA_Any (Result_Ü.Argument));
   end get_couleur;

   procedure set_couleur
     (Self : Ref;
      To : in mod1.Int1.Color)
   is
      --  Prepare in arguments

      Arg_Name_Ü_To : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("To");
      Argument_Ü_To : CORBA.Any
        := mod1.Int1.Helper.To_Any
        (To);
      Operation_Name_Ü : constant Standard.String
        := "_set_couleur";
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
        (CORBA.Internals.Get_Empty_Any (CORBA.TC_Void)),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => CORBA.Object.Internals.To_PolyORB_Ref
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
         PolyORB.Requests.Destroy_Request (Request_Ü);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any (Result_Ü.Argument);

         --  Not reached

      end if;
      PolyORB.Requests.Destroy_Request (Request_Ü);

      --  Request has been synchronously invoked.
   end set_couleur;

   function get_b1
     (Self : Ref)
     return mod1.bool
   is
      --  Prepare in arguments


      Operation_Name_Ü : constant Standard.String
        := "_get_b1";
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
        (CORBA.Internals.Get_Empty_Any (mod1.Helper.TC_bool)),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => CORBA.Object.Internals.To_PolyORB_Ref
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
         PolyORB.Requests.Destroy_Request (Request_Ü);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any (Result_Ü.Argument);

         --  Not reached

      end if;
      PolyORB.Requests.Destroy_Request (Request_Ü);

      --  Request has been synchronously invoked.

      --  Retrieve return value

      return mod1.Helper.From_Any
        (CORBA.Internals.To_CORBA_Any (Result_Ü.Argument));
   end get_b1;

   procedure set_b1
     (Self : Ref;
      To : in mod1.bool)
   is
      --  Prepare in arguments

      Arg_Name_Ü_To : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("To");
      Argument_Ü_To : CORBA.Any
        := mod1.Helper.To_Any
        (To);
      Operation_Name_Ü : constant Standard.String
        := "_set_b1";
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
        (CORBA.Internals.Get_Empty_Any (CORBA.TC_Void)),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => CORBA.Object.Internals.To_PolyORB_Ref
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
         PolyORB.Requests.Destroy_Request (Request_Ü);
         PolyORB.CORBA_P.Exceptions.Raise_From_Any (Result_Ü.Argument);

         --  Not reached

      end if;
      PolyORB.Requests.Destroy_Request (Request_Ü);

      --  Request has been synchronously invoked.
   end set_b1;

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
         mod1.Int1.Repository_Id)
        or else CORBA.Is_Equivalent
          (Logical_Type_Id,
           "IDL:omg.org/CORBA/Object:1.0")
        or else False;

   end Is_A;

end mod1.Int1;
