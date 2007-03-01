------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  C O R B A . D O M A I N M A N A G E R                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2007, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.Any.NVList;
with PolyORB.Requests;
with PolyORB.Types;

with CORBA.Helper;
with CORBA.Policy.Helper;

with PolyORB.CORBA_P.Exceptions;
with PolyORB.CORBA_P.Interceptors_Hooks;

package body CORBA.DomainManager is

   -----------------------
   -- Get_Domain_Policy --
   -----------------------

   function Get_Domain_Policy
     (Self        : Ref;
      Policy_Type : PolicyType)
      return CORBA.Policy.Ref
   is
      Operation_Name : constant Standard.String := "get_domain_policy";

      Arg_Name_Policy_Type : PolyORB.Types.Identifier
        := PolyORB.Types.To_PolyORB_String ("policy_type");
      Argument_Policy_Type : CORBA.Any
        := CORBA.Helper.To_Any (Policy_Type);
      Self_Ref             : CORBA.Object.Ref := CORBA.Object.Ref (Self);
      Request              : PolyORB.Requests.Request_Access;
      Arg_List             : PolyORB.Any.NVList.Ref;
      Result               : PolyORB.Any.NamedValue;
      Result_Name          : CORBA.String := To_CORBA_String ("Result");

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      PolyORB.Any.NVList.Create (Arg_List);
      PolyORB.Any.NVList.Add_Item
        (Arg_List,
         Arg_Name_Policy_Type,
         CORBA.Internals.To_PolyORB_Any (Argument_Policy_Type),
         PolyORB.Any.ARG_IN);

      Result :=
        (Name      => PolyORB.Types.Identifier (Result_Name),
         Argument  => CORBA.Internals.To_PolyORB_Any
         (CORBA.Internals.Get_Empty_Any (CORBA.Policy.Helper.TC_Policy)),
         Arg_Modes => 0);

      PolyORB.Requests.Create_Request
        (Target    => CORBA.Object.Internals.To_PolyORB_Ref
         (CORBA.Object.Ref (Self)),
         Operation => Operation_Name,
         Arg_List  => Arg_List,
         Result    => Result,
         Req       => Request);

      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request,
         PolyORB.Requests.Flags (0));

      PolyORB.CORBA_P.Exceptions.Request_Raise_Occurrence (Request);

      PolyORB.Requests.Destroy_Request (Request);

      return
        CORBA.Policy.Helper.From_Any
        (CORBA.Internals.To_CORBA_Any (Result.Argument));
   end Get_Domain_Policy;

   ----------
   -- Is_A --
   ----------

   function Is_A
     (Self            : Ref;
      Logical_Type_Id : Standard.String)
      return CORBA.Boolean
   is
   begin
      return Is_A (Logical_Type_Id)
        or else Object.Is_A (Object.Ref (Self), Logical_Type_Id);
   end Is_A;

   function Is_A (Logical_Type_Id : Standard.String) return CORBA.Boolean is
   begin
      return Is_Equivalent (Logical_Type_Id, Repository_Id)
        or else Is_Equivalent
        (Logical_Type_Id, "IDL:omg.org/CORBA/Object:1.0");
   end Is_A;

end CORBA.DomainManager;
