------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        C O R B A . C O N T E X T                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2006, Free Software Foundation, Inc.          --
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

package body CORBA.Context is

   -------------------
   -- Set_One_Value --
   -------------------

   procedure Set_One_Value
     (Self      : Ref;
      Prop_Name : Identifier;
      Value     : CORBA.String)
   is
      pragma Unreferenced
        (Self,
         Prop_Name,
         Value);
   begin
      CORBA.Raise_No_Implement (CORBA.Default_Sys_Member);
   end Set_One_Value;

   ----------------
   -- Set_Values --
   ----------------

   procedure Set_Values
     (Self   : Ref;
      Values : CORBA.NVList.Ref)
   is
      pragma Warnings (Off);
      pragma Unreferenced
        (Self,
         Values);
      pragma Warnings (On);
   begin
      CORBA.Raise_No_Implement (CORBA.Default_Sys_Member);
   end Set_Values;

   ----------------
   -- Get_Values --
   ----------------

   procedure Get_Values
     (Self        : Ref;
      Start_Scope : Identifier;
      This_Object : Boolean := True;
      Prop_Name   : Identifier;
      Values      :    out CORBA.NVList.Ref)
   is
      pragma Unreferenced
        (Self,
         Start_Scope,
         This_Object,
         Prop_Name);

      Dummy : CORBA.NVList.Ref;
      pragma Warnings (Off, Dummy);
      --  No explicit initialization.

   begin
      Values := Dummy;
      CORBA.Raise_No_Implement (CORBA.Default_Sys_Member);
   end Get_Values;

   -------------------
   -- Delete_Values --
   -------------------

   procedure Delete_Values
     (Self      : Ref;
      Prop_Name : Identifier)
   is
      pragma Unreferenced
        (Self,
         Prop_Name);
   begin
      CORBA.Raise_No_Implement (CORBA.Default_Sys_Member);
   end Delete_Values;

   ------------------
   -- Create_Child --
   ------------------

   procedure Create_Child
     (Self      : Ref;
      Ctx_Name  : Identifier;
      Child_Ctx :    out Ref)
   is
      pragma Unreferenced
        (Self,
         Ctx_Name);

      Dummy : Ref;
      pragma Warnings (Off, Dummy);
      --  No explicit initialization.

   begin
      Child_Ctx := Dummy;
      CORBA.Raise_No_Implement (CORBA.Default_Sys_Member);
   end Create_Child;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Self      : Ref;
      Del_Flags : Flags)
   is
      pragma Unreferenced
        (Self,
         Del_Flags);
   begin
      CORBA.Raise_No_Implement (CORBA.Default_Sys_Member);
   end Delete;

end CORBA.Context;
