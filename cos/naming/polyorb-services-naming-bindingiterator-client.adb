------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            C O S N A M I N G . B I N D I N G I T E R A T O R             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2002 Free Software Foundation, Inc.             --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

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
      Arg_Name_�_b : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("b");
      Argument_�_b : CORBA.Any
        := CosNaming.Helper.To_Any
        (b);

      Operation_Name_� : constant Standard.String
        := "next_one";
      Self_Ref_� : CORBA.Object.Ref
        := CORBA.Object.Ref (Self);

      Request_� : PolyORB.Requests.Request_Access;
      Arg_List_� : PolyORB.Any.NVList.Ref;
      Result_� : PolyORB.Any.NamedValue;
      Result_Name_� : CORBA.String := To_CORBA_String ("Result");
   begin

      if CORBA.Object.Is_Nil (Self_Ref_�) then
         PolyORB.Exceptions.Raise_Inv_Objref;
      end if;

      --  Create argument list
      PolyORB.Any.NVList.Create
        (Arg_List_�);
      PolyORB.Any.NVList.Add_Item
        (Arg_List_�,
         Arg_Name_�_b,
         Argument_�_b,
         PolyORB.Any.ARG_OUT);
      --  Set result type (maybe void)
      Result_�
        := (Name => PolyORB.Types.Identifier (Result_Name_�),
            Argument => Get_Empty_Any
        (CORBA.TC_Boolean),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => CORBA.Object.To_PolyORB_Ref
         (CORBA.Object.Ref (Self)),
         Operation => Operation_Name_�,
         Arg_List  => Arg_List_�,
         Result    => Result_�,
         Req       => Request_�);

      PolyORB.Requests.Invoke (Request_�);
      if not Is_Empty (Request_�.Exception_Info) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Any
           (Request_�.Exception_Info);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request_�);

      --  Request has been synchronously invoked.

      --  Retrieve return value.
      Returns := CORBA.From_Any
        (Result_�.Argument);

      --  Retrieve 'out' argument values.

      b := CosNaming.Helper.From_Any
        (Argument_�_b);
   end next_one;

   procedure next_n
     (Self : Ref;
      how_many : in CORBA.Unsigned_Long;
      bl : out CosNaming.BindingList;
      Returns : out CORBA.Boolean)
   is
      Arg_Name_�_how_many : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("how_many");
      Argument_�_how_many : CORBA.Any
        := CORBA.To_Any
        (how_many);
      Arg_Name_�_bl : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("bl");
      Argument_�_bl : CORBA.Any
        := CosNaming.Helper.To_Any
        (bl);

      Operation_Name_� : constant Standard.String
        := "next_n";
      Self_Ref_� : CORBA.Object.Ref
        := CORBA.Object.Ref (Self);

      Request_� : PolyORB.Requests.Request_Access;
      Arg_List_� : PolyORB.Any.NVList.Ref;
      Result_� : PolyORB.Any.NamedValue;
      Result_Name_� : CORBA.String := To_CORBA_String ("Result");
   begin

      if CORBA.Object.Is_Nil (Self_Ref_�) then
         PolyORB.Exceptions.Raise_Inv_Objref;
      end if;

      --  Create argument list
      PolyORB.Any.NVList.Create
        (Arg_List_�);
      PolyORB.Any.NVList.Add_Item
        (Arg_List_�,
         Arg_Name_�_how_many,
         Argument_�_how_many,
         PolyORB.Any.ARG_IN);
      PolyORB.Any.NVList.Add_Item
        (Arg_List_�,
         Arg_Name_�_bl,
         Argument_�_bl,
         PolyORB.Any.ARG_OUT);
      --  Set result type (maybe void)
      Result_�
        := (Name => PolyORB.Types.Identifier (Result_Name_�),
            Argument => Get_Empty_Any
        (CORBA.TC_Boolean),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => CORBA.Object.To_PolyORB_Ref
         (CORBA.Object.Ref (Self)),
         Operation => Operation_Name_�,
         Arg_List  => Arg_List_�,
         Result    => Result_�,
         Req       => Request_�);

      PolyORB.Requests.Invoke (Request_�);
      if not Is_Empty (Request_�.Exception_Info) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Any
           (Request_�.Exception_Info);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request_�);

      --  Request has been synchronously invoked.

      --  Retrieve return value.
      Returns := CORBA.From_Any
        (Result_�.Argument);

      --  Retrieve 'out' argument values.

      bl := CosNaming.Helper.From_Any
        (Argument_�_bl);
   end next_n;

   procedure destroy
     (Self : Ref)
   is

      Operation_Name_� : constant Standard.String
        := "destroy";
      Self_Ref_� : CORBA.Object.Ref
        := CORBA.Object.Ref (Self);

      Request_� : PolyORB.Requests.Request_Access;
      Arg_List_� : PolyORB.Any.NVList.Ref;
      Result_� : PolyORB.Any.NamedValue;
      Result_Name_� : CORBA.String := To_CORBA_String ("Result");
   begin

      if CORBA.Object.Is_Nil (Self_Ref_�) then
         PolyORB.Exceptions.Raise_Inv_Objref;
      end if;

      --  Create argument list
      PolyORB.Any.NVList.Create
        (Arg_List_�);
      --  Set result type (maybe void)
      Result_�
        := (Name => PolyORB.Types.Identifier (Result_Name_�),
            Argument => Get_Empty_Any
        (CORBA.TC_Void),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => CORBA.Object.To_PolyORB_Ref
         (CORBA.Object.Ref (Self)),
         Operation => Operation_Name_�,
         Arg_List  => Arg_List_�,
         Result    => Result_�,
         Req       => Request_�);

      PolyORB.Requests.Invoke (Request_�);
      if not Is_Empty (Request_�.Exception_Info) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Any
           (Request_�.Exception_Info);
      end if;
      PolyORB.Requests.Destroy_Request
        (Request_�);

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
