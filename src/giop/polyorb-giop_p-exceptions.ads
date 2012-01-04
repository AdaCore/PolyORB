------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--            P O L Y O R B . G I O P _ P . E X C E P T I O N S             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

--  Exceptions management for the GIOP Protocol Personality of PolyORB.

with PolyORB.Any;

package PolyORB.GIOP_P.Exceptions is

   function Is_System_Exception (Name : String) return Boolean;

   function To_CORBA_Exception
     (Exc : PolyORB.Any.Any)
     return PolyORB.Any.Any;
   --  Convert PolyORB Exc exception typecode to CORBA exception typecode

   function Extract_System_Exception_Name
     (Name : Standard.String)
     return Standard.String;
   --  Extract the name of the system exception found in Name.

   function System_Exception_TypeCode
     (Name : Standard.String)
     return PolyORB.Any.TypeCode.Local_Ref;
   --  Return the TypeCode corresponding to the indicated
   --  system exception name.

end PolyORB.GIOP_P.Exceptions;
