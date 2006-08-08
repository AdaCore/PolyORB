------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      C O S N A M I N G . N A M I N G C O N T E X T E X T . I M P L       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2006, Free Software Foundation, Inc.             --
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
