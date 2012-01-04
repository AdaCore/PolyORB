------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . E R R O R S . H E L P E R                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Any;

package PolyORB.Errors.Helper is

   -----------------------
   -- Completion_Status --
   -----------------------

   To_Completion_Status :
     constant array (PolyORB.Types.Unsigned_Long range 0 .. 2)
         of Completion_Status
     := (0 => Completed_Yes, 1 => Completed_No, 2 => Completed_Maybe);

   To_Unsigned_Long :
     constant array (Completion_Status) of PolyORB.Types.Unsigned_Long
     := (Completed_Yes => 0, Completed_No => 1, Completed_Maybe => 2);

   function From_Any (Item : PolyORB.Any.Any) return Completion_Status;

   function To_Any (Item : Completion_Status) return Any.Any;

   function TC_Completion_Status return PolyORB.Any.TypeCode.Local_Ref;
   --  The typecode for standard enumeration type completion_status

   --  Null_Members

   function To_Any
     (Name   : Standard.String;
      Member : Null_Members)
     return PolyORB.Any.Any;

   --  System_Exception_Members

   function System_Exception_TypeCode
     (Name : Standard.String)
     return PolyORB.Any.TypeCode.Local_Ref;
   --  Return the TypeCode corresponding to the indicated
   --  system exception name.

   function From_Any (Item : Any.Any) return System_Exception_Members;

   function To_Any
     (Name   : Standard.String;
      Member : System_Exception_Members)
     return PolyORB.Any.Any;

   --  Standard exceptions

   function TC_Comm_Failure return PolyORB.Any.TypeCode.Local_Ref;

   function TC_Transient return PolyORB.Any.TypeCode.Local_Ref;

   function TC_No_Response return PolyORB.Any.TypeCode.Local_Ref;

   function TC_Obj_Adapter return PolyORB.Any.TypeCode.Local_Ref;

   --  ForwardRequest_Members

   function To_Any
     (Item : ForwardRequest_Members)
      return PolyORB.Any.Any;

   function From_Any
     (Item : PolyORB.Any.Any)
      return ForwardRequest_Members;

   function TC_ForwardRequest return PolyORB.Any.TypeCode.Local_Ref;

   --  ForwardRequestPerm_Members

   function To_Any
     (Item : ForwardRequestPerm_Members)
      return PolyORB.Any.Any;

   function From_Any
     (Item : PolyORB.Any.Any)
      return ForwardRequestPerm_Members;

   function TC_ForwardRequestPerm return PolyORB.Any.TypeCode.Local_Ref;

   --  NeedsAddressingMode_Members

   function To_Any
     (Item : NeedsAddressingMode_Members)
      return PolyORB.Any.Any;

   function From_Any
     (Item : PolyORB.Any.Any)
      return NeedsAddressingMode_Members;

   function TC_NeedsAddressingMode return PolyORB.Any.TypeCode.Local_Ref;

   function Error_To_Any (Error : Error_Container) return PolyORB.Any.Any;

end PolyORB.Errors.Helper;
