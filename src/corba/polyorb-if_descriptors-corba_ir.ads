------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . I F _ D E S C R I P T O R S . C O R B A _ I R       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2002 Free Software Foundation, Inc.             --
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

--  An Interface Descriptor that uses the CORBA Interface Repository.

--  $Id$

package PolyORB.If_Descriptors.CORBA_IR is

   type IR_If_Descriptor is new If_Descriptor with private;

   function Get_Empty_Arg_List
     (If_Desc : access IR_If_Descriptor;
      Object  :        PolyORB.References.Ref;
      Method  :        String)
     return Any.NVList.Ref;

   function Get_Empty_Result
     (If_Desc : access IR_If_Descriptor;
      Object  :        PolyORB.References.Ref;
      Method  :        String)
     return Any.Any;

private

   type IR_If_Descriptor is new If_Descriptor
     with null record;

end PolyORB.If_Descriptors.CORBA_IR;
