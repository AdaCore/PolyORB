------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                       C O R B A . C O N T E X T                          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
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

with CORBA.NVList;

package CORBA.Context is

   type Ref is private;

   procedure Set_One_Value
     (Self      : in Ref;
      Prop_Name : in Identifier;
      Value     : in CORBA.String);

   procedure Set_Values
     (Self   : in Ref;
      Values : in CORBA.NVList.Ref);

   procedure Get_Values
     (Self        : in     Ref;
      Start_Scope : in     Identifier;
      This_Object : in     Boolean := TRUE;
      Prop_Name   : in     Identifier;
      Values      :    out CORBA.NVList.Ref);

   procedure Delete_Values
     (Self      : in Ref;
      Prop_Name : in Identifier);

   procedure Create_Child
     (Self      : in     Ref;
      Ctx_Name  : in     Identifier;
      Child_Ctx :    out Ref);

   procedure Delete
     (Self       : in Ref;
      Del_Flagfs : in Flags);

private

   type Ref is null record;

end CORBA.Context;
