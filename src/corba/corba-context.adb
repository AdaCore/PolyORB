------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        C O R B A . C O N T E X T                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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

--  $Id$

package body CORBA.Context is

   procedure Set_One_Value
     (Self      : in Ref;
      Prop_Name : in Identifier;
      Value     : in CORBA.String) is
   begin
      null;
   end Set_One_Value;

   procedure Set_Values
     (Self   : in Ref;
      Values : in CORBA.NVList.Ref) is
   begin
      null;
   end Set_Values;

   procedure Get_Values
     (Self        : in     Ref;
      Start_Scope : in     Identifier;
      This_Object : in     Boolean := True;
      Prop_Name   : in     Identifier;
      Values      :    out CORBA.NVList.Ref)
   is
      Dummy : CORBA.NVList.Ref;
      pragma Warnings (Off, Dummy);
   begin
      Values := Dummy;
   end Get_Values;

   procedure Delete_Values
     (Self      : in Ref;
      Prop_Name : in Identifier) is
   begin
      null;
   end Delete_Values;

   procedure Create_Child
     (Self      : in     Ref;
      Ctx_Name  : in     Identifier;
      Child_Ctx :    out Ref)
   is
      Dummy : Ref;
      pragma Warnings (Off, Dummy);
   begin
      Child_Ctx := Dummy;
   end Create_Child;

   procedure Delete
     (Self       : in Ref;
      Del_Flagfs : in Flags) is
   begin
      null;
   end Delete;

end CORBA.Context;
