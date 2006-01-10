------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--             D Y N A M I C A N Y . D Y N A N Y F A C T O R Y              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
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

with Ada.Exceptions;

with CORBA.Object;

with DynamicAny.DynAny;

package DynamicAny.DynAnyFactory is

   type Local_Ref is new CORBA.Object.Ref with null record;

   --  InconsistentTypeCode exception

   InconsistentTypeCode : exception;

   type InconsistentTypeCode_Members is
     new CORBA.IDL_Exception_Members with null record;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out InconsistentTypeCode_Members);

   function Create_Dyn_Any
     (Self  : Local_Ref;
      Value : CORBA.Any)
      return DynAny.Local_Ref;

   function Create_Dyn_Any_From_Type_Code
     (Self     : Local_Ref;
      IDL_Type : CORBA.TypeCode.Object)
      return DynAny.Local_Ref;

   function Create_Dyn_Any_Without_Truncation
     (Self  : Local_Ref;
      Value : CORBA.Any)
      return DynAny.Local_Ref;

   function Create_Multiple_Dyn_Anys
     (Self           : Local_Ref;
      Values         : AnySeq;
      Allow_Truncate : CORBA.Boolean)
      return DynAnySeq;

   function Create_Multiple_Anys
     (Self   : Local_Ref;
      Values : DynamicAny.DynAnySeq)
      return DynamicAny.AnySeq;

   --  Repository Ids

   DynAnyFactory_Root_Repository_Id                : constant Standard.String
     := "IDL:omg.org/DynamicAny/DynAnyFactory";

   Repository_Id                                   : constant Standard.String
     := DynAnyFactory_Root_Repository_Id & ":1.0";

   Create_Dyn_Any_Repository_Id                    : constant Standard.String
     := DynAnyFactory_Root_Repository_Id & "/create_dyn_any:1.0";

   Create_Dyn_Any_From_Type_Code_Repository_Id     : constant Standard.String
     := DynAnyFactory_Root_Repository_Id
     & "/create_dyn_any_from_type_code:1.0";

   Create_Dyn_Any_Without_Truncation_Repository_Id : constant Standard.String
     := DynAnyFactory_Root_Repository_Id
     & "/create_dyn_any_without_truncation:1.0";

   Create_Multiple_Anys_Repository_Id              : constant Standard.String
     := DynAnyFactory_Root_Repository_Id & "/create_multiple_anys:1.0";

   Create_Multiple_Dyn_Anys_Repository_Id          : constant Standard.String
     := DynAnyFactory_Root_Repository_Id & "/create_multiple_dyn_anys:1.0";

   InconsistentTypeCode_Repository_Id              : constant Standard.String
     := DynAnyFactory_Root_Repository_Id & "/InconsistentTypeCode:1.0";

end DynamicAny.DynAnyFactory;
