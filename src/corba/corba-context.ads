------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                        C O R B A . C O N T E X T                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitly   --
-- nor implicitly specified by the CORBA Specification defined by the OMG.  --
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

with CORBA.NVList;

package CORBA.Context is

   type Ref is private;
   Nil_Ref : constant Ref;

   procedure Set_One_Value
     (Self      : Ref;
      Prop_Name : Identifier;
      Value     : CORBA.String);

   procedure Set_Values
     (Self   : Ref;
      Values : CORBA.NVList.Ref);

   procedure Get_Values
     (Self        : Ref;
      Start_Scope : Identifier;
      This_Object : Boolean := True;
      Prop_Name   : Identifier;
      Values      :    out CORBA.NVList.Ref);

   procedure Delete_Values
     (Self      : Ref;
      Prop_Name : Identifier);

   procedure Create_Child
     (Self      : Ref;
      Ctx_Name  : Identifier;
      Child_Ctx :    out Ref);

   procedure Delete
     (Self      : Ref;
      Del_Flags : Flags);

private

   type Ref is null record;
   Nil_Ref : constant Ref := (null record);

end CORBA.Context;
