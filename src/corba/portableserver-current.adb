------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O R T A B L E S E R V E R . C U R R E N T                --
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

--  $Id$

package body PortableServer.Current is

   -------------
   -- Get_POA --
   -------------

   function Get_POA
     (Self : Ref)
     return PortableServer.POA_Forward.Ref is
   begin
      pragma Warnings (Off);
      return Get_POA (Self);
      pragma Warnings (On);
   end Get_POA;

   -------------------
   -- Get_Object_Id --
   -------------------

   function Get_Object_Id
     (Self : Ref)
     return ObjectId is
   begin
      pragma Warnings (Off);
      return Get_Object_Id (Self);
      pragma Warnings (On);
   end Get_Object_Id;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out NoContext_Members)
   is
      use Ada.Exceptions;

   begin
      if Exception_Identity (From) /= NoContext'Identity then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      To := NoContext_Members'
        (CORBA.IDL_Exception_Members with null record);
   end Get_Members;

   ---------------------
   -- Raise_NoContext --
   ---------------------

   procedure Raise_NoContext
     (Excp_Memb : in NoContext_Members)
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Excp_Memb);
      pragma Warnings (On); --  WAG:3.15
   begin
      raise NoContext;
   end Raise_NoContext;

end PortableServer.Current;
