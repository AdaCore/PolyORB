with PolyORB.Any.NVList;
with PolyORB.Types;
with CORBA;
with CORBA.Object;
with PolyORB.Requests;
with PolyORB.Any;
with PolyORB.CORBA_P.Interceptors_Hooks;
with PolyORB.CORBA_P.Exceptions;
with PolyORB.References;
with PolyORB.Smart_Pointers;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Strings.Lists;
with PolyORB.Initialization;
with PolyORB.Any.ExceptionList;
with PolyORB.Request_QOS;

package body Harness is

   procedure echoULong_Cache_Init;

   procedure echoULong_Get_Request
     (Arg_List : in PolyORB.Any.NVList.Ref;
      Ref      : in PolyORB.References.Ref;
      Result   : in PolyORB.Any.NamedValue;
      Request  : out PolyORB.Requests.Request_Access;
      Index    : out Natural);
   pragma Inline (echoULong_Get_Request);

   procedure echoULong_Free_Request
     (Index : Natural);
   pragma Inline (echoULong_Free_Request);

   echoULong_Arg_Name_U_arg : PolyORB.Types.Identifier :=
     PolyORB.Types.To_PolyORB_String
        ("arg");

   echoULong_Operation_Name_ü : constant Standard.String :=
     "echoULong";

   echoULong_Result_Name_ü : CORBA.String :=
     CORBA.To_CORBA_String
     ("Result");

   Request_Cache_Tab : array (1..10) of PolyORB.Requests.Request_Access;
   Request_Cache_Tab_Free : array (1..10) of Boolean
     := (others => True);

   --------------------------
   -- echoULong_Result_U_V --
   --------------------------

   function echoULong_Result_U_V return PolyORB.Any.NamedValue is
      pragma Inline (echoULong_Result_U_V);
   begin
      return
        (Name => PolyORB.Types.Identifier
         (echoULong_Result_Name_ü),
         Argument => CORBA.Internals.To_PolyORB_Any
         (CORBA.Get_Empty_Any
          (CORBA.TC_Unsigned_Long)),
         Arg_Modes => 0);
   end echoULong_Result_U_V;

   ---------------
   -- echoULong --
   ---------------

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
      Result_ü : PolyORB.Any.NamedValue
        := echoULong_Result_U_V;
      Index : Natural;
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


      --  Get the request from the cache

      EchoULong_Get_Request
        (Argument_List_Ü,
         CORBA.Object.To_PolyORB_Ref
         (CORBA.Object.Ref
          (Self)),
         Result_Ü,
         Request_Ü,
         Index);

      --  Invoke the operation on the server

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

      --   free the request

      EchoULong_Free_Request (Index);

      return CORBA.From_Any
        (CORBA.Internals.To_CORBA_Any
           (Result_ü.Argument));
   end echoULong;

   ---------------------------
   -- echoULong_Get_Request --
   ---------------------------

   procedure echoULong_Get_Request
     (Arg_List : in PolyORB.Any.NVList.Ref;
      Ref      : in PolyORB.References.Ref;
      Result   : in PolyORB.Any.NamedValue;
      Request  : out PolyORB.Requests.Request_Access;
      Index    : out Natural)
   is
      I : Natural := 1;
   begin
      --  There is always a free request in the cache.
      --  A solution with mutex will be developped to handle the concurency problem.

      loop
         exit when Request_Cache_Tab_Free (I);
         I := I + 1;
      end loop;

      Request_Cache_Tab (I).all.Target := Ref;
      Request_Cache_Tab (I).all.Args := Arg_List;
      Request_Cache_Tab (I).all.Result := Result;
      --  Request_Cache_Tab_Free (I) := False;

      --  Set returns values

      Index := I;
      Request :=  Request_Cache_Tab (I);
   end echoULong_Get_Request;

   ----------------------------
   -- echoULong_Free_Request --
   ----------------------------

   procedure echoULong_Free_Request
     (Index : Natural)
   is
   begin
      --  Request_Cache_Tab (Index).all.Req_Flags
      --  := PolyORB.Requests.Default_Flags;
      --  Request_Cache_Tab (Index).all.Deferred_Arguments_Session
      --  := null;
      --  Request_Cache_Tab (Index).all.Args_Ident
      --  := PolyORB.Requests.Ident_By_Position;
      Request_Cache_Tab (Index).all.Exc_List
        := PolyORB.Any.ExceptionList.Nil_Ref;
      Request_Cache_Tab (Index).all.Completed
        := False;
      Request_Cache_Tab (Index).all.Arguments_Called
        := False;

      --  PolyORB.Request_QoS.Set_QoS
      --  (Request_Cache_Tab (Index),
      --   PolyORB.Request_QoS.Fetch_QoS
      --   (Request_Cache_Tab (Index).all.Target));

      Request_Cache_Tab_Free (Index) := True;
   end echoULong_Free_Request;

   --------------------------
   -- echoULong_Cache_Init --
   --------------------------

   procedure echoULong_Cache_Init is
      Operation_Name_ü : constant Standard.String :=
        "echoULong";
      Result_U     : PolyORB.Any.NamedValue
        := echoULong_Result_U_V;
      Arg_List_Nil : PolyORB.Any.NVList.Ref;
   begin

      for I in 1..10 loop
         PolyORB.Requests.Create_Request
           (Target => PolyORB.References.Nil_Ref,
            Operation => Operation_Name_Ü,
            Arg_List => Arg_List_Nil,
            Result => Result_U,
            Req      => Request_Cache_Tab (I));
      end loop;

   end echoULong_Cache_Init;

   ----------
   -- Is_A --
   ----------

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

   ----------
   -- Is_A --
   ----------

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

   procedure Deferred_Initialization
   is
   begin
      echoULong_Cache_Init;
   end Deferred_Initialization;

begin
   declare
      List : PolyORB.Utils.Strings.Lists.List;
   begin
      PolyORB.Utils.Strings.Lists.Append (List, "smart_pointers");
      PolyORB.Initialization.Register_Module
        (PolyORB.Initialization.Module_Info'
         (Name => PolyORB.Utils.Strings."+"
          ("Harness"),
          Conflicts => PolyORB.Utils.Strings.Lists.Empty,
          Depends => List,
          Provides => PolyORB.Utils.Strings.Lists.Empty,
          Implicit => False,
          Init => Deferred_Initialization'Access));
   end;

end Harness;
