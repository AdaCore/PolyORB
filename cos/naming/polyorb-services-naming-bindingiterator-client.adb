----------------------------------------------
--  This file has been generated automatically
--  by IDLAC (http://www.polyorb.eu.org/)
--
--  Do NOT hand-modify this file, as your
--  changes will be lost when you re-run the
--  IDL to Ada compiler.
----------------------------------------------
pragma Warnings (Off);

with CosNaming.Helper;
with CORBA.Object;
with PolyORB.Types;
with PolyORB.Requests;
with PolyORB.Any.NVList;
with PolyORB.CORBA_P.Exceptions;
with PolyORB.Exceptions;
with CORBA; use CORBA;
pragma Elaborate_All (CORBA);

package body CosNaming.BindingIterator is

   procedure next_one
     (Self : Ref;
      b : out CosNaming.Binding;
      Returns : out CORBA.Boolean)
   is
      Arg_Name_Ü_b : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("b");
      Argument_Ü_b : CORBA.Any
        := CosNaming.Helper.To_Any
        (b);

      Operation_Name_Ü : constant Standard.String
        := "next_one";
      Self_Ref_Ü : CORBA.Object.Ref
        := CORBA.Object.Ref (Self);

      Request_Ü : PolyORB.Requests.Request_Access;
      Arg_List_Ü : PolyORB.Any.NVList.Ref;
      Result_Ü : PolyORB.Any.NamedValue;
      Result_Name_Ü : CORBA.String := To_CORBA_String ("Result");
   begin

      if CORBA.Object.Is_Nil (Self_Ref_Ü) then
         PolyORB.Exceptions.Raise_Inv_Objref;
      end if;

      --  Create argument list
      PolyORB.Any.NVList.Create
        (Arg_List_Ü);
      PolyORB.Any.NVList.Add_Item
        (Arg_List_Ü,
         Arg_Name_Ü_b,
         Argument_Ü_b,
         PolyORB.Any.ARG_OUT);
      --  Set result type (maybe void)
      Result_Ü
        := (Name => PolyORB.Types.Identifier (Result_Name_Ü),
            Argument => Get_Empty_Any
        (CORBA.TC_Boolean),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => CORBA.Object.To_PolyORB_Ref
         (CORBA.Object.Ref (Self)),
         Operation => Operation_Name_Ü,
         Arg_List  => Arg_List_Ü,
         Result    => Result_Ü,
         Req       => Request_Ü);

      PolyORB.Requests.Invoke (Request_Ü);
      if not Is_Empty (Request_Ü.Exception_Info) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Any
           (Request_Ü.Exception_Info);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request_Ü);

      --  Request has been synchronously invoked.

      --  Retrieve return value.
      Returns := CORBA.From_Any
        (Result_Ü.Argument);

      --  Retrieve 'out' argument values.

      b := CosNaming.Helper.From_Any
        (Argument_Ü_b);
   end next_one;

   procedure next_n
     (Self : Ref;
      how_many : in CORBA.Unsigned_Long;
      bl : out CosNaming.BindingList;
      Returns : out CORBA.Boolean)
   is
      Arg_Name_Ü_how_many : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("how_many");
      Argument_Ü_how_many : CORBA.Any
        := CORBA.To_Any
        (how_many);
      Arg_Name_Ü_bl : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("bl");
      Argument_Ü_bl : CORBA.Any
        := CosNaming.Helper.To_Any
        (bl);

      Operation_Name_Ü : constant Standard.String
        := "next_n";
      Self_Ref_Ü : CORBA.Object.Ref
        := CORBA.Object.Ref (Self);

      Request_Ü : PolyORB.Requests.Request_Access;
      Arg_List_Ü : PolyORB.Any.NVList.Ref;
      Result_Ü : PolyORB.Any.NamedValue;
      Result_Name_Ü : CORBA.String := To_CORBA_String ("Result");
   begin

      if CORBA.Object.Is_Nil (Self_Ref_Ü) then
         PolyORB.Exceptions.Raise_Inv_Objref;
      end if;

      --  Create argument list
      PolyORB.Any.NVList.Create
        (Arg_List_Ü);
      PolyORB.Any.NVList.Add_Item
        (Arg_List_Ü,
         Arg_Name_Ü_how_many,
         Argument_Ü_how_many,
         PolyORB.Any.ARG_IN);
      PolyORB.Any.NVList.Add_Item
        (Arg_List_Ü,
         Arg_Name_Ü_bl,
         Argument_Ü_bl,
         PolyORB.Any.ARG_OUT);
      --  Set result type (maybe void)
      Result_Ü
        := (Name => PolyORB.Types.Identifier (Result_Name_Ü),
            Argument => Get_Empty_Any
        (CORBA.TC_Boolean),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => CORBA.Object.To_PolyORB_Ref
         (CORBA.Object.Ref (Self)),
         Operation => Operation_Name_Ü,
         Arg_List  => Arg_List_Ü,
         Result    => Result_Ü,
         Req       => Request_Ü);

      PolyORB.Requests.Invoke (Request_Ü);
      if not Is_Empty (Request_Ü.Exception_Info) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Any
           (Request_Ü.Exception_Info);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request_Ü);

      --  Request has been synchronously invoked.

      --  Retrieve return value.
      Returns := CORBA.From_Any
        (Result_Ü.Argument);

      --  Retrieve 'out' argument values.

      bl := CosNaming.Helper.From_Any
        (Argument_Ü_bl);
   end next_n;

   procedure destroy
     (Self : Ref)
   is

      Operation_Name_Ü : constant Standard.String
        := "destroy";
      Self_Ref_Ü : CORBA.Object.Ref
        := CORBA.Object.Ref (Self);

      Request_Ü : PolyORB.Requests.Request_Access;
      Arg_List_Ü : PolyORB.Any.NVList.Ref;
      Result_Ü : PolyORB.Any.NamedValue;
      Result_Name_Ü : CORBA.String := To_CORBA_String ("Result");
   begin

      if CORBA.Object.Is_Nil (Self_Ref_Ü) then
         PolyORB.Exceptions.Raise_Inv_Objref;
      end if;

      --  Create argument list
      PolyORB.Any.NVList.Create
        (Arg_List_Ü);
      --  Set result type (maybe void)
      Result_Ü
        := (Name => PolyORB.Types.Identifier (Result_Name_Ü),
            Argument => Get_Empty_Any
        (CORBA.TC_Void),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => CORBA.Object.To_PolyORB_Ref
         (CORBA.Object.Ref (Self)),
         Operation => Operation_Name_Ü,
         Arg_List  => Arg_List_Ü,
         Result    => Result_Ü,
         Req       => Request_Ü);

      PolyORB.Requests.Invoke (Request_Ü);
      if not Is_Empty (Request_Ü.Exception_Info) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Any
           (Request_Ü.Exception_Info);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request_Ü);

      --  Request has been synchronously invoked.
   end destroy;

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
         CosNaming.BindingIterator.Repository_Id)
        or else CORBA.Is_Equivalent
          (Logical_Type_Id,
           "IDL:omg.org/CORBA/Object:1.0")
        or else False;

   end Is_A;

end CosNaming.BindingIterator;
