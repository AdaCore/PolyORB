------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  C O R B A . D O M A I N M A N A G E R                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2005;

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

      Arg_Name_Policy_Type : constant PolyORB.Types.Identifier :=
        PolyORB.Types.To_PolyORB_String ("policy_type");
      Argument_Policy_Type : constant CORBA.Any :=
        CORBA.Helper.To_Any (Policy_Type);
      Self_Ref             : constant CORBA.Object.Ref :=
        CORBA.Object.Ref (Self);
      Request              : aliased PolyORB.Requests.Request;
      Arg_List             : PolyORB.Any.NVList.Ref;
      Result               : PolyORB.Any.NamedValue;
      Result_Name          : constant CORBA.String :=
        To_CORBA_String ("Result");

   begin
      if CORBA.Object.Is_Nil (Self_Ref) then
         CORBA.Raise_Inv_Objref (CORBA.Default_Sys_Member);
      end if;

      PolyORB.Any.NVList.Create (Arg_List);
      PolyORB.Any.NVList.Add_Item
        (Arg_List,
         Arg_Name_Policy_Type,
         PolyORB.Any.Any (Argument_Policy_Type),
         PolyORB.Any.ARG_IN);

      Result :=
        (Name      => PolyORB.Types.Identifier (Result_Name),
         Argument  => CORBA.Internals.Get_Empty_Any
                        (CORBA.Policy.Helper.TC_Policy),
         Arg_Modes => 0);

      PolyORB.Requests.Setup_Request
        (Req       => Request,
         Target    => CORBA.Object.Internals.To_PolyORB_Ref
                        (CORBA.Object.Ref (Self)),
         Operation => Operation_Name,
         Arg_List  => Arg_List,
         Result    => Result);

      PolyORB.CORBA_P.Interceptors_Hooks.Client_Invoke
        (Request'Access,
         PolyORB.Requests.Flags (0));

      PolyORB.CORBA_P.Exceptions.Request_Raise_Occurrence (Request);
      return CORBA.Policy.Helper.From_Any (CORBA.Any (Result.Argument));
   end Get_Domain_Policy;

   ----------
   -- Is_A --
   ----------

   overriding function Is_A
     (Self            : Ref;
      Logical_Type_Id : Standard.String) return CORBA.Boolean
   is
   begin
      return Is_A (Logical_Type_Id)
               or else Object.Is_A (Object.Ref (Self), Logical_Type_Id);
   end Is_A;

   function Is_A (Logical_Type_Id : Standard.String) return CORBA.Boolean is
   begin
      return Is_Equivalent (Logical_Type_Id, Repository_Id)
               or else
             Is_Equivalent (Logical_Type_Id, "IDL:omg.org/CORBA/Object:1.0");
   end Is_A;

end CORBA.DomainManager;
