with PolyORB.Any.NVList;
with PolyORB.Types;
with CORBA;
with CORBA.Object;
with PolyORB.Requests;
with PolyORB.Any;
with PolyORB.CORBA_P.Interceptors_Hooks;
with PolyORB.CORBA_P.Exceptions;
with PolyORB.References;
with PolyORB.Utils.Strings;
with PolyORB.Utils.Strings.Lists;
with PolyORB.Initialization;
with PolyORB.Any.ExceptionList;

package body Harness is

   procedure echoULong_Cache_Init;

   procedure echoULong_Get_Request
     (Ref      : in PolyORB.References.Ref;
      Result   : in PolyORB.Any.NamedValue;
      Request  : out PolyORB.Requests.Request_Access;
      Index    : out Natural);
   pragma Inline (echoULong_Get_Request);

   procedure echoULong_Free_Request (Index : Natural);
   pragma Inline (echoULong_Free_Request);

   procedure EchoULong_Update_Argument_List
     (Request        :    PolyORB.Requests.Request_Access;
      Argument_U_Arg : in CORBA.Any);
   pragma Inline (EchoULong_Update_Argument_List);

   echoULong_Operation_Name_ü : constant Standard.String :=
     "echoULong";

   echoULong_Result_Name_ü : constant CORBA.String :=
     CORBA.To_CORBA_String
     ("Result");

   Cache_Size : constant := 10;

   Request_Cache_Tab : array (1 .. Cache_Size)
     of PolyORB.Requests.Request_Access;

   Request_Cache_Tab_Free : array (1 .. Cache_Size) of Boolean
     := (others => True);

   --  XXX tout ceci devrait etre prefixé par echoULong ..

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

      Argument_U_arg : CORBA.Any
        := CORBA.To_Any
           (arg);
      Self_Ref_ü : CORBA.Object.Ref :=
         CORBA.Object.Ref
           (Self);
      Request_ü : PolyORB.Requests.Request_Access;
      Result_ü : PolyORB.Any.NamedValue
        := echoULong_Result_U_V;

      --  XXX est ce que cette variable est necessaire ?

      Index : Natural;
   begin
      if CORBA.Object.Is_Nil
        (Self_Ref_ü)
      then
         CORBA.Raise_Inv_Objref
           (CORBA.Default_Sys_Member);
      end if;

      --  Get the request from the cache

      EchoULong_Get_Request
        (CORBA.Object.To_PolyORB_Ref
         (CORBA.Object.Ref
          (Self)),
         Result_Ü,
         Request_Ü,
         Index);

      --  Make Arg List

      EchoULong_Update_Argument_List (Request_Ü, Argument_U_Arg);

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

         EchoULong_Free_Request (Index);

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
     (Ref      : in PolyORB.References.Ref;
      Result   : in PolyORB.Any.NamedValue;
      Request  : out PolyORB.Requests.Request_Access;
      Index    : out Natural)
   is
      I : Natural := 1;
   begin
      --  There is always a free request in the cache.

      --  A solution with mutex will be developped to handle the
      --  concurency problem.

      --  Enter (echoULong_Mutex);

      loop
         exit when Request_Cache_Tab_Free (I);
         I := I + 1;
      end loop;
      Index := I;

      --  Leave (echoULong_Mutex);
      --  cf par ex PolyORB.Smart_Pointers pour plus de details

      Request_Cache_Tab (Index).all.Target := Ref;
--      Request_Cache_Tab (Index).all.Args := EchoULong_Argument_List;
      Request_Cache_Tab (Index).all.Result := Result;
      Request_Cache_Tab_Free (Index) := False;

      --  Set returns values

      Request :=  Request_Cache_Tab (Index);
   end echoULong_Get_Request;

   ----------------------------
   -- echoULong_Free_Request --
   ----------------------------

   procedure EchoULong_Free_Request (Index : Natural)
   is
   begin
      Request_Cache_Tab (Index).all.Exc_List
        := PolyORB.Any.ExceptionList.Nil_Ref;
      Request_Cache_Tab (Index).all.Completed
        := False;
      Request_Cache_Tab (Index).all.Arguments_Called
        := False;

      Request_Cache_Tab_Free (Index) := True;
   end echoULong_Free_Request;

   --------------------------
   -- echoULong_Cache_Init --
   --------------------------

   procedure echoULong_Cache_Init is
      Operation_Name_ü : constant Standard.String :=
        "echoULong";
      echoULong_Arg_Name_U_arg : PolyORB.Types.Identifier :=
        PolyORB.Types.To_PolyORB_String
        ("arg");
      Result_U     : PolyORB.Any.NamedValue
        := echoULong_Result_U_V;
      Arg_List_Nil : PolyORB.Any.NVList.Ref;
      Argument_U_Arg : CORBA.Any;

      echoULong_Argument_List : PolyORB.Any.NVList.Ref;

   begin

      PolyORB.Any.NVList.Create
        (echoULong_Argument_List);

      PolyORB.Any.NVList.Add_Item
        (echoULong_Argument_List,
         echoULong_Arg_Name_U_arg,
         CORBA.Internals.To_PolyORB_Any
           (Argument_U_arg),
         PolyORB.Any.ARG_IN);

      for I in Request_Cache_Tab'Range loop
         PolyORB.Requests.Create_Request
           (Target => PolyORB.References.Nil_Ref,
            Operation => Operation_Name_Ü,
            Arg_List => echoULong_Argument_List,
            Result => Result_U,
            Req      => Request_Cache_Tab (I));
      end loop;

   end echoULong_Cache_Init;

   ------------------------------------
   -- EchoULong_Update_Argument_List --
   ------------------------------------

   procedure EchoULong_Update_Argument_List
     (Request        :    PolyORB.Requests.Request_Access;
      Argument_U_Arg : in CORBA.Any)
   is
      List_Ptr : PolyORB.Any.NVList.Internals.NV_List_Access
        := PolyORB.Any.NVList.Internals.List_Of
        (Request.Args);
      Element : PolyORB.Any.NVList.Internals.NV_Lists.Element_Access;

   begin
      Element :=
        PolyORB.Any.NVList.Internals.NV_Lists.Element
        (List_Ptr.all, 0);
      Element.all.Argument := CORBA.Internals.To_PolyORB_Any
        (Argument_U_Arg);
   end EchoULong_Update_Argument_List;

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
