------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              POLYORB.SERVICES.NAMING.NAMINGCONTEXT.CLIENT                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  with PolyORB.Services.Naming.BindingIterator;
--  with PolyORB.Services.Naming.BindingIterator.Helper;
with PolyORB.Services.Naming.NamingContext.Helper;
with PolyORB.Services.Naming.Helper;

with PolyORB.Types;
with PolyORB.Requests;
with PolyORB.Any;
with PolyORB.Any.ExceptionList;
with PolyORB.Any.NVList;
with PolyORB.Any.ObjRef;
with PolyORB.Exceptions;

package body PolyORB.Services.Naming.NamingContext.Client is

   use PolyORB.Any;
   use PolyORB.Any.ExceptionList;
   use PolyORB.Any.NVList;
   use PolyORB.Any.ObjRef;
   use PolyORB.Types;

   --   use PolyORB.Services.Naming.BindingIterator;
   use PolyORB.Services.Naming.Helper;
   use PolyORB.Services.Naming.NamingContext.Helper;
   use PolyORB.Services.Naming.Helper;

   ----------
   -- Bind --
   ----------

   procedure Bind
     (Self : PolyORB.Services.Naming.NamingContext.Ref;
      N    : in Name;
      Obj  : in PolyORB.References.Ref)
   is
      Arg_Name_N : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("n");
      Argument_N : PolyORB.Any.Any := To_Any (N);
      Arg_Name_Obj : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("obj");
      Argument_Obj : PolyORB.Any.Any :=
        PolyORB.Services.Naming.Helper.To_Any (Obj);
      Operation_Name : constant Standard.String
        := "bind";

      Request : PolyORB.Requests.Request_Access;
      Arg_List : PolyORB.Any.NVList.Ref;
      Excp_List : PolyORB.Any.ExceptionList.Ref;
      Result : PolyORB.Any.NamedValue;
      Result_Name : PolyORB.Types.String := To_PolyORB_String ("Result");
   begin

      --  Create argument list
      PolyORB.Any.NVList.Create (Arg_List);

      PolyORB.Any.NVList.Add_Item
        (Arg_List,
         Arg_Name_N,
         Argument_N,
         PolyORB.Any.ARG_IN);
      PolyORB.Any.NVList.Add_Item
        (Arg_List,
         Arg_Name_Obj,
         Argument_Obj,
         PolyORB.Any.ARG_IN);

      --  Create exceptions list.

      Create_List (Excp_List);
      Add
        (Excp_List,
         TC_NotFound);
      Add
        (Excp_List,
         TC_CannotProceed);
      Add
        (Excp_List,
         TC_InvalidName);
      Add
        (Excp_List,
         TC_AlreadyBound);
      --  Set result type (maybe void)
      Result
        := (Name => PolyORB.Types.Identifier (Result_Name),
            Argument => Get_Empty_Any
        (PolyORB.Any.TypeCode.TC_Void),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => PolyORB.References.Ref (Self),
         Operation => Operation_Name,
         Arg_List  => Arg_List,
         Result    => Result,
         Exc_List  => Excp_List,
         Req       => Request);

      PolyORB.Requests.Invoke (Request);
      if not Is_Empty (Request.Exception_Info) then
         PolyORB.Exceptions.Raise_From_Any
           (Request.Exception_Info);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request);

      --  Request has been synchronously invoked.
   end Bind;

   ------------
   -- Rebind --
   ------------

   procedure Rebind
     (Self : PolyORB.Services.Naming.NamingContext.Ref;
      N    : in Name;
      Obj  : in PolyORB.References.Ref)
   is
      Arg_Name_n : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("n");
      Argument_n : PolyORB.Any.Any
        := To_Any (N);
      Arg_Name_obj : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("obj");
      Argument_obj : PolyORB.Any.Any
        := PolyORB.Services.Naming.Helper.To_Any (Obj);

      Operation_Name : constant Standard.String
        := "rebind";

      Request : PolyORB.Requests.Request_Access;
      Arg_List : PolyORB.Any.NVList.Ref;
      Excp_List : PolyORB.Any.ExceptionList.Ref;
      Result : PolyORB.Any.NamedValue;
      Result_Name : PolyORB.Types.String := To_PolyORB_String ("Result");
   begin

      --  Create argument list
      PolyORB.Any.NVList.Create
        (Arg_List);
      PolyORB.Any.NVList.Add_Item
        (Arg_List,
         Arg_Name_n,
         Argument_n,
         PolyORB.Any.ARG_IN);
      PolyORB.Any.NVList.Add_Item
        (Arg_List,
         Arg_Name_obj,
         Argument_obj,
         PolyORB.Any.ARG_IN);

      --  Create exceptions list.

      Create_List (Excp_List);
      Add
        (Excp_List,
         TC_NotFound);
      Add
        (Excp_List,
         TC_CannotProceed);
      Add
        (Excp_List,
         TC_InvalidName);
      --  Set result type (maybe void)
      Result
        := (Name => PolyORB.Types.Identifier (Result_Name),
            Argument => Get_Empty_Any
        (PolyORB.Any.TypeCode.TC_Void),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => PolyORB.References.Ref (Self),
         Operation => Operation_Name,
         Arg_List  => Arg_List,
         Result    => Result,
         Exc_List  => Excp_List,
         Req       => Request);

      PolyORB.Requests.Invoke (Request);
      if not Is_Empty (Request.Exception_Info) then
         PolyORB.Exceptions.Raise_From_Any
           (Request.Exception_Info);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request);

      --  Request has been synchronously invoked.
   end Rebind;

   ------------------
   -- Bind_Context --
   ------------------

   procedure Bind_Context
     (Self : PolyORB.Services.Naming.NamingContext.Ref;
      N    : in Name;
      Nc   : in NamingContext.Ref)
   is
      Arg_Name_n : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("n");
      Argument_n : PolyORB.Any.Any
        := To_Any (N);
      Arg_Name_nc : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("nc");
      Argument_nc : PolyORB.Any.Any
        := To_Any (Nc);

      Operation_Name : constant Standard.String
        := "bind_context";

      Request : PolyORB.Requests.Request_Access;
      Arg_List : PolyORB.Any.NVList.Ref;
      Excp_List : PolyORB.Any.ExceptionList.Ref;
      Result : PolyORB.Any.NamedValue;
      Result_Name : PolyORB.Types.String := To_PolyORB_String ("Result");
   begin

      --  Create argument list
      PolyORB.Any.NVList.Create
        (Arg_List);
      PolyORB.Any.NVList.Add_Item
        (Arg_List,
         Arg_Name_n,
         Argument_n,
         PolyORB.Any.ARG_IN);
      PolyORB.Any.NVList.Add_Item
        (Arg_List,
         Arg_Name_nc,
         Argument_nc,
         PolyORB.Any.ARG_IN);

      --  Create exceptions list.

      Create_List (Excp_List);
      Add
        (Excp_List,
         TC_NotFound);
      Add
        (Excp_List,
         TC_CannotProceed);
      Add
        (Excp_List,
         TC_InvalidName);
      Add
        (Excp_List,
         TC_AlreadyBound);
      --  Set result type (maybe void)
      Result
        := (Name => PolyORB.Types.Identifier (Result_Name),
            Argument => Get_Empty_Any
        (PolyORB.Any.TypeCode.TC_Void),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => PolyORB.References.Ref (Self),
         Operation => Operation_Name,
         Arg_List  => Arg_List,
         Result    => Result,
         Exc_List  => Excp_List,
         Req       => Request);

      PolyORB.Requests.Invoke (Request);
      if not Is_Empty (Request.Exception_Info) then
         PolyORB.Exceptions.Raise_From_Any
           (Request.Exception_Info);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request);

      --  Request has been synchronously invoked.
   end Bind_Context;

   --------------------
   -- Rebind_Context --
   --------------------

   procedure Rebind_Context
     (Self : PolyORB.Services.Naming.NamingContext.Ref;
      N    : in Name;
      Nc   : in NamingContext.Ref)
   is
      Arg_Name_n : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("n");
      Argument_n : PolyORB.Any.Any
        := To_Any (N);
      Arg_Name_nc : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("nc");
      Argument_nc : PolyORB.Any.Any
        := To_Any (Nc);

      Operation_Name : constant Standard.String
        := "rebind_context";

      Request : PolyORB.Requests.Request_Access;
      Arg_List : PolyORB.Any.NVList.Ref;
      Excp_List : PolyORB.Any.ExceptionList.Ref;
      Result : PolyORB.Any.NamedValue;
      Result_Name : PolyORB.Types.String := To_PolyORB_String ("Result");
   begin

      --  Create argument list
      PolyORB.Any.NVList.Create
        (Arg_List);
      PolyORB.Any.NVList.Add_Item
        (Arg_List,
         Arg_Name_n,
         Argument_n,
         PolyORB.Any.ARG_IN);
      PolyORB.Any.NVList.Add_Item
        (Arg_List,
         Arg_Name_nc,
         Argument_nc,
         PolyORB.Any.ARG_IN);

      --  Create exceptions list.

      Create_List (Excp_List);
      Add
        (Excp_List,
         TC_NotFound);
      Add
        (Excp_List,
         TC_CannotProceed);
      Add
        (Excp_List,
         TC_InvalidName);
      --  Set result type (maybe void)
      Result
        := (Name => PolyORB.Types.Identifier (Result_Name),
            Argument => Get_Empty_Any
        (PolyORB.Any.TypeCode.TC_Void),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => PolyORB.References.Ref (Self),
         Operation => Operation_Name,
         Arg_List  => Arg_List,
         Result    => Result,
         Exc_List  => Excp_List,
         Req       => Request);

      PolyORB.Requests.Invoke (Request);
      if not Is_Empty (Request.Exception_Info) then
         PolyORB.Exceptions.Raise_From_Any
           (Request.Exception_Info);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request);

      --  Request has been synchronously invoked.
   end Rebind_Context;

   -------------
   -- Resolve --
   -------------

   function Resolve
     (Self : PolyORB.Services.Naming.NamingContext.Ref;
      N    : in Name)
     return PolyORB.References.Ref
   is
      Arg_Name_n : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("n");
      Argument_n : PolyORB.Any.Any
        := To_Any (N);

      Operation_Name : constant Standard.String
        := "resolve";

      Request : PolyORB.Requests.Request_Access;
      Arg_List : PolyORB.Any.NVList.Ref;
      Excp_List : PolyORB.Any.ExceptionList.Ref;
      Result : PolyORB.Any.NamedValue;
      Result_Name : PolyORB.Types.String := To_PolyORB_String ("Result");
   begin

      --  Create argument list
      PolyORB.Any.NVList.Create
        (Arg_List);
      PolyORB.Any.NVList.Add_Item
        (Arg_List,
         Arg_Name_n,
         Argument_n,
         PolyORB.Any.ARG_IN);

      --  Create exceptions list.

      Create_List (Excp_List);
      Add
        (Excp_List,
         TC_NotFound);
      Add
        (Excp_List,
         TC_CannotProceed);
      Add
        (Excp_List,
         TC_InvalidName);
      --  Set result type (maybe void)
      Result
        := (Name => PolyORB.Types.Identifier (Result_Name),
            Argument => Get_Empty_Any
            (PolyORB.Services.Naming.Helper.TC_Object),
            Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => PolyORB.References.Ref (Self),
         Operation => Operation_Name,
         Arg_List  => Arg_List,
         Result    => Result,
         Exc_List  => Excp_List,
         Req       => Request);

      PolyORB.Requests.Invoke (Request);
      if not Is_Empty (Request.Exception_Info) then
         PolyORB.Exceptions.Raise_From_Any
           (Request.Exception_Info);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request);

      --  Request has been synchronously invoked.

      --  Retrieve return value.
      return From_Any
        (Result.Argument);
   end Resolve;

   ------------
   -- Unbind --
   ------------

   procedure Unbind
     (Self : PolyORB.Services.Naming.NamingContext.Ref;
      N    : in Name)
   is
      Arg_Name_n : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("n");
      Argument_n : PolyORB.Any.Any
        := To_Any (N);

      Operation_Name : constant Standard.String
        := "unbind";

      Request : PolyORB.Requests.Request_Access;
      Arg_List : PolyORB.Any.NVList.Ref;
      Excp_List : PolyORB.Any.ExceptionList.Ref;
      Result : PolyORB.Any.NamedValue;
      Result_Name : PolyORB.Types.String := To_PolyORB_String ("Result");
   begin

      --  Create argument list
      PolyORB.Any.NVList.Create
        (Arg_List);
      PolyORB.Any.NVList.Add_Item
        (Arg_List,
         Arg_Name_n,
         Argument_n,
         PolyORB.Any.ARG_IN);

      --  Create exceptions list.

      Create_List (Excp_List);
      Add
        (Excp_List,
         TC_NotFound);
      Add
        (Excp_List,
         TC_CannotProceed);
      Add
        (Excp_List,
         TC_InvalidName);
      --  Set result type (maybe void)
      Result
        := (Name => PolyORB.Types.Identifier (Result_Name),
            Argument => Get_Empty_Any
        (PolyORB.Any.TypeCode.TC_Void),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => PolyORB.References.Ref (Self),
         Operation => Operation_Name,
         Arg_List  => Arg_List,
         Result    => Result,
         Exc_List  => Excp_List,
         Req       => Request);

      PolyORB.Requests.Invoke (Request);
      if not Is_Empty (Request.Exception_Info) then
         PolyORB.Exceptions.Raise_From_Any
           (Request.Exception_Info);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request);

      --  Request has been synchronously invoked.
   end Unbind;

   -----------------
   -- New_Context --
   -----------------

   function New_Context
     (Self : PolyORB.Services.Naming.NamingContext.Ref)
     return NamingContext.Ref
   is

      Operation_Name : constant Standard.String
        := "new_context";

      Request : PolyORB.Requests.Request_Access;
      Arg_List : PolyORB.Any.NVList.Ref;
      Result : PolyORB.Any.NamedValue;
      Result_Name : PolyORB.Types.String := To_PolyORB_String ("Result");
   begin

      --  Create argument list
      PolyORB.Any.NVList.Create
        (Arg_List);
      --  Set result type (maybe void)
      Result
        := (Name => PolyORB.Types.Identifier (Result_Name),
            Argument => Get_Empty_Any
        (TC_NamingContext),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => PolyORB.References.Ref (Self),
         Operation => Operation_Name,
         Arg_List  => Arg_List,
         Result    => Result,
         Req       => Request);

      PolyORB.Requests.Invoke (Request);
      if not Is_Empty (Request.Exception_Info) then
         PolyORB.Exceptions.Raise_From_Any
           (Request.Exception_Info);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request);

      --  Request has been synchronously invoked.

      --  Retrieve return value.
      return From_Any
        (Result.Argument);
   end New_Context;

   ----------------------
   -- Bind_New_Context --
   ----------------------

   function Bind_New_Context
     (Self : PolyORB.Services.Naming.NamingContext.Ref;
      N    : in Name)
     return NamingContext.Ref
   is
      Arg_Name_n : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("n");
      Argument_n : PolyORB.Any.Any
        := To_Any (N);

      Operation_Name : constant Standard.String
        := "bind_new_context";

      Request : PolyORB.Requests.Request_Access;
      Arg_List : PolyORB.Any.NVList.Ref;
      Excp_List : PolyORB.Any.ExceptionList.Ref;
      Result : PolyORB.Any.NamedValue;
      Result_Name : PolyORB.Types.String := To_PolyORB_String ("Result");
   begin

      --  Create argument list
      PolyORB.Any.NVList.Create
        (Arg_List);
      PolyORB.Any.NVList.Add_Item
        (Arg_List,
         Arg_Name_n,
         Argument_n,
         PolyORB.Any.ARG_IN);

      --  Create exceptions list.

      Create_List (Excp_List);
      Add
        (Excp_List,
         TC_NotFound);
      Add
        (Excp_List,
         TC_AlreadyBound);
      Add
        (Excp_List,
         TC_CannotProceed);
      Add
        (Excp_List,
         TC_InvalidName);
      --  Set result type (maybe void)
      Result
        := (Name => PolyORB.Types.Identifier (Result_Name),
            Argument => Get_Empty_Any
        (TC_NamingContext),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => PolyORB.References.Ref (Self),
         Operation => Operation_Name,
         Arg_List  => Arg_List,
         Result    => Result,
         Exc_List  => Excp_List,
         Req       => Request);

      PolyORB.Requests.Invoke (Request);
      if not Is_Empty (Request.Exception_Info) then
         PolyORB.Exceptions.Raise_From_Any
           (Request.Exception_Info);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request);

      --  Request has been synchronously invoked.

      --  Retrieve return value.
      return From_Any
        (Result.Argument);
   end Bind_New_Context;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Self : PolyORB.Services.Naming.NamingContext.Ref)
   is

      Operation_Name : constant Standard.String
        := "destroy";

      Request : PolyORB.Requests.Request_Access;
      Arg_List : PolyORB.Any.NVList.Ref;
      Excp_List : PolyORB.Any.ExceptionList.Ref;
      Result : PolyORB.Any.NamedValue;
      Result_Name : PolyORB.Types.String := To_PolyORB_String ("Result");
   begin

      --  Create argument list
      PolyORB.Any.NVList.Create
        (Arg_List);

      --  Create exceptions list.

      Create_List (Excp_List);
      Add
        (Excp_List,
         TC_NotEmpty);
      --  Set result type (maybe void)
      Result
        := (Name => PolyORB.Types.Identifier (Result_Name),
            Argument => Get_Empty_Any
        (PolyORB.Any.TypeCode.TC_Void),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => PolyORB.References.Ref (Self),
         Operation => Operation_Name,
         Arg_List  => Arg_List,
         Result    => Result,
         Exc_List  => Excp_List,
         Req       => Request);

      PolyORB.Requests.Invoke (Request);
      if not Is_Empty (Request.Exception_Info) then
         PolyORB.Exceptions.Raise_From_Any
           (Request.Exception_Info);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request);

      --  Request has been synchronously invoked.
   end Destroy;

--     procedure list
--       (Self : PolyORB.Services.Naming.Ref;
--        how_many : in CORBA.Unsigned_Long;
--        bl : out BindingList;
--        bi : out BindingIterator_Forward.Ref)
--     is
--        Arg_Name_how_many : PolyORB.Types.Identifier
--          := PolyORB.Types.To_PolyORB_String ("how_many");
--        Argument_how_many : PolyORB.Any.Any
--          := CORBA.To_Any
--          (how_many);
--        Arg_Name_bl : PolyORB.Types.Identifier
--          := PolyORB.Types.To_PolyORB_String ("bl");
--        Argument_bl : PolyORB.Any.Any
--          := To_Any
--          (bl);
--        Arg_Name_bi : PolyORB.Types.Identifier
--          := PolyORB.Types.To_PolyORB_String ("bi");
--        Argument_bi : PolyORB.Any.Any
--          := To_Any
--          (Convert_Forward.From_Forward
--          (bi));

--        Operation_Name : constant Standard.String
--          := "list";
--        Self_Ref : PolyORB.References.Ref
--          := PolyORB.References.Ref (Self);

--        Request : PolyORB.Requests.Request_Access;
--        Arg_List : PolyORB.Any.NVList.Ref;
--        Result : PolyORB.Any.NamedValue;
--        Result_Name : PolyORB.Types.String := To_PolyORB_String ("Result");
--     begin

--        if CORBA.Object.Is_Nil (Self_Ref) then
--           PolyORB.Exceptions.Raise_Inv_Objref;
--        end if;

--        --  Create argument list
--        PolyORB.Any.NVList.Create
--          (Arg_List);
--        PolyORB.Any.NVList.Add_Item
--          (Arg_List,
--           Arg_Name_how_many,
--           Argument_how_many,
--           PolyORB.Any.ARG_IN);
--        PolyORB.Any.NVList.Add_Item
--          (Arg_List,
--           Arg_Name_bl,
--           Argument_bl,
--           PolyORB.Any.ARG_OUT);
--        PolyORB.Any.NVList.Add_Item
--          (Arg_List,
--           Arg_Name_bi,
--           Argument_bi,
--           PolyORB.Any.ARG_OUT);
--        --  Set result type (maybe void)
--        Result
--          := (Name => PolyORB.Types.Identifier (Result_Name),
--              Argument => Get_Empty_Any
--          (PolyORB.Any.TypeCode.TC_Void),
--           Arg_Modes => 0);

--        PolyORB.Requests.Create_Request
--          (Target    => CORBA.Object.To_PolyORB_Ref
--           (PolyORB.References.Ref (Self)),
--           Operation => Operation_Name,
--           Arg_List  => Arg_List,
--           Result    => Result,
--           Req       => Request);

--        PolyORB.Requests.Invoke (Request);
--        if not Is_Empty (Request.Exception_Info) then
--           PolyORB.CORBA_P.Exceptions.Raise_From_Any
--             (Request.Exception_Info);
--        end if;
--        PolyORB.Requests.Destroy_Request
--          (Request);

--        --  Request has been synchronously invoked.

--        --  Retrieve 'out' argument values.

--        bl := From_Any
--          (Argument_bl);
--        bi := Convert_Forward.To_Forward
--          (From_Any
--          (Argument_bi));
--     end list;

end PolyORB.Services.Naming.NamingContext.Client;
