------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                        C O R B A . C O N T E X T                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--         Copyright (C) 1999-2000 ENST Paris University, France.           --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with CORBA.NVList; use CORBA.NVList;

package body CORBA.Context is

   procedure Set_One_Value
     (Self      : in out Object;
      Prop_Name : in    Identifier;
      Value     : in    CORBA.String)
   is
   begin
      null;
   end Set_One_Value;

   ----------------
   -- Set_Values --
   ----------------

   procedure Set_Values
     (Self      : in out Object;
      Values    : in     CORBA.NVList.Object)
   is
   begin
      null;
   end Set_Values;

   ----------------
   -- Get_Values --
   ----------------

   procedure Get_Values
     (Self        : in     Object;
      Start_Scope : in     Identifier;
      This_Object : in     Boolean := True;
      Prop_Name   : in     Identifier;
      Values      :    out CORBA.NVList.Object)
   is
   begin
      Values := Null_Object;
   end Get_Values;

   -------------------
   -- Delete_Values --
   -------------------

   procedure Delete_Values
     (Self      : in out Object;
      Prop_Name : in     Identifier)
   is
   begin
      null;
   end Delete_Values;

   ------------------
   -- Create_Child --
   ------------------

   procedure Create_Child
     (Self      : in out Object;
      Ctx_Name  : in     Identifier;
      Child_Ctx :    out Object)
   is
   begin
      Child_Ctx := 0;
   end Create_Child;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Self              : in Object;
      Delete_Descendant : in Boolean := False)
   is
   begin
      null;
   end Delete;

end CORBA.Context;
