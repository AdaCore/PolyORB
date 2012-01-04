------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          P O L Y O R B . C O R B A _ P . D Y N A M I C _ A N Y           --
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

with CORBA;

with DynamicAny.DynAny.Impl;

package PolyORB.CORBA_P.Dynamic_Any is

   function Create
     (Value          : CORBA.Any;
      Allow_Truncate : Boolean;
      Parent         : DynamicAny.DynAny.Impl.Object_Ptr)
      return DynamicAny.DynAny.Local_Ref;
   --  Create new DynAny object and initialize it to specified value

   function Create
     (IDL_Type : CORBA.TypeCode.Object)
      return DynamicAny.DynAny.Local_Ref;
   --  Create new DynAny object and initialize it to default value

end PolyORB.CORBA_P.Dynamic_Any;
