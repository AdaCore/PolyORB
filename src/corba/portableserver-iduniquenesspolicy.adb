------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--    P O R T A B L E S E R V E R . I D U N I Q U E N E S S P O L I C Y     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
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

with PolyORB.CORBA_P.Policy;

with PolyORB.Smart_Pointers;

package body PortableServer.IdUniquenessPolicy is

   use CORBA.Policy;

   use PolyORB.CORBA_P.Policy;

   ------------
   -- To_Ref --
   ------------

   function To_Ref
     (The_Ref : in CORBA.Object.Ref'Class)
     return Ref
   is
      use type CORBA.PolicyType;

   begin
      if The_Ref not in CORBA.Policy.Ref'Class
        or else Get_Policy_Type (CORBA.Policy.Ref (The_Ref)) /=
        ID_UNIQUENESS_POLICY_ID
      then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      declare
         Entity : constant PolyORB.Smart_Pointers.Entity_Ptr :=
           new Policy_Object_Type;

         Result : Ref;
      begin
         Set_Policy_Type (Policy_Object_Type (Entity.all),
                          ID_UNIQUENESS_POLICY_ID);

         Set_Policy_Value (Policy_Object_Type (Entity.all),
                           Get_Policy_Value
                           (Policy_Object_Type
                            (Entity_Of
                             (CORBA.Policy.Ref (The_Ref)).all)));

         CORBA.Policy.Set (CORBA.Policy.Ref (Result), Entity);

         return Result;
      end;
   end To_Ref;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Self : Ref)
     return PortableServer.IdUniquenessPolicyValue is
   begin
      return From_Any (Get_Policy_Value
                       (Policy_Object_Type
                        (Entity_Of
                         (CORBA.Policy.Ref (Self)).all)));
   end Get_Value;

end PortableServer.IdUniquenessPolicy;
