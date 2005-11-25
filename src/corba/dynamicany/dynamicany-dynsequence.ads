------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               D Y N A M I C A N Y . D Y N S E Q U E N C E                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2005 Free Software Foundation, Inc.             --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with CORBA;

with DynamicAny.DynAny;

package DynamicAny.DynSequence is

   type Local_Ref is new DynamicAny.DynAny.Local_Ref with null record;

   function Get_Length (Self : in Local_Ref) return CORBA.Unsigned_Long;

   procedure Set_Length
     (Self : in Local_Ref;
      Len  : in CORBA.Unsigned_Long);

   function Get_Elements (Self : in Local_Ref) return AnySeq;

   procedure Set_Elements
     (Self  : in Local_Ref;
      Value : in AnySeq);

   function Get_Elements_As_Dyn_Any (Self : in Local_Ref) return DynAnySeq;

   procedure Set_Elements_As_Dyn_Any
     (Self  : in Local_Ref;
      Value : in DynAnySeq);

   --  Repository Ids

   Repository_Id                         : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynSequence:1.0";

   Get_Elements_Repository_Id            : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynSequence/get_elements:1.0";

   Get_Elements_As_Dyn_Any_Repository_Id : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynSequence/get_elements_as_dyn_any:1.0";

   Get_Length_Repository_Id              : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynSequence/get_length:1.0";

   Set_Elements_Repository_Id            : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynSequence/set_elements:1.0";

   Set_Elements_As_Dyn_Any_Repository_Id : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynSequence/set_elements_as_dyn_any:1.0";

   Set_Length_Repository_Id              : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynSequence/set_length:1.0";

end DynamicAny.DynSequence;
