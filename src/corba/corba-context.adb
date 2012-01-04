------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        C O R B A . C O N T E X T                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
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
