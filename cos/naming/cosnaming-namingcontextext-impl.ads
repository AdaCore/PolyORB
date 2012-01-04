------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      C O S N A M I N G . N A M I N G C O N T E X T E X T . I M P L       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2006-2012, Free Software Foundation, Inc.          --
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

with CORBA.Object;
with CosNaming.NamingContext.Impl;
pragma Elaborate_All (CosNaming.NamingContext.Impl);

package CosNaming.NamingContextExt.Impl is

   type Object is new CosNaming.NamingContext.Impl.Object with private;
   type Object_Ptr is access all Object'Class;

   function To_String
     (Self : access Object;
      N : CosNaming.Name)
     return CosNaming.NamingContextExt.StringName;

   function To_Name
     (Self : access Object;
      Sn : CosNaming.NamingContextExt.StringName)
     return CosNaming.Name;

   function To_Url
     (Self : access Object;
      Addr : CosNaming.NamingContextExt.Address;
      Sn : CosNaming.NamingContextExt.StringName)
     return CosNaming.NamingContextExt.URLString;

   function Resolve_Str
     (Self : access Object;
      Sn : CosNaming.NamingContextExt.StringName)
     return CORBA.Object.Ref;

   function Create return CosNaming.NamingContextExt.Impl.Object_Ptr;
   procedure Initialize (Self : Object_Ptr);

private

   type Object is new CosNaming.NamingContext.Impl.Object with null record;

end CosNaming.NamingContextExt.Impl;
