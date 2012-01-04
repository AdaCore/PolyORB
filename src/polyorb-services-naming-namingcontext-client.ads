------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              POLYORB.SERVICES.NAMING.NAMINGCONTEXT.CLIENT                --
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

with PolyORB.References;

package PolyORB.Services.Naming.NamingContext.Client is

   procedure Bind
     (Self : PolyORB.Services.Naming.NamingContext.Ref;
      N    : Name;
      Obj  : PolyORB.References.Ref);

   Bind_Repository_Id : constant Standard.String
     := "IDL:omg.org/CosNaming/NamingContext/bind:1.0";

   procedure Rebind
     (Self : PolyORB.Services.Naming.NamingContext.Ref;
      N    : Name;
      Obj  : PolyORB.References.Ref);

   Rebind_Repository_Id : constant Standard.String
     := "IDL:omg.org/CosNaming/NamingContext/rebind:1.0";

   procedure Bind_Context
     (Self : PolyORB.Services.Naming.NamingContext.Ref;
      N    : Name;
      Nc   : NamingContext.Ref);

   Bind_Context_Repository_Id : constant Standard.String
     := "IDL:omg.org/CosNaming/NamingContext/bind_context:1.0";

   procedure Rebind_Context
     (Self : PolyORB.Services.Naming.NamingContext.Ref;
      N    : Name;
      Nc   : NamingContext.Ref);

   Rebind_Context_Repository_Id : constant Standard.String
     := "IDL:omg.org/CosNaming/NamingContext/rebind_context:1.0";

   function Resolve
     (Self : PolyORB.Services.Naming.NamingContext.Ref;
      N    : Name)
     return PolyORB.References.Ref;

   Resolve_Repository_Id : constant Standard.String
     := "IDL:omg.org/CosNaming/NamingContext/resolve:1.0";

   procedure Unbind
     (Self : PolyORB.Services.Naming.NamingContext.Ref;
      N    : Name);

   Unbind_Repository_Id : constant Standard.String
     := "IDL:omg.org/CosNaming/NamingContext/unbind:1.0";

   function New_Context
     (Self : PolyORB.Services.Naming.NamingContext.Ref)
     return NamingContext.Ref;

   New_Context_Repository_Id : constant Standard.String
     := "IDL:omg.org/CosNaming/NamingContext/new_context:1.0";

   function Bind_New_Context
     (Self : PolyORB.Services.Naming.NamingContext.Ref;
      N    : Name)
     return NamingContext.Ref;

   Bind_New_Context_Repository_Id : constant Standard.String
     := "IDL:omg.org/CosNaming/NamingContext/bind_new_context:1.0";

   procedure Destroy
     (Self : PolyORB.Services.Naming.NamingContext.Ref);

   Destroy_Repository_Id : constant Standard.String
     := "IDL:omg.org/CosNaming/NamingContext/destroy:1.0";

--     procedure List
--       (Self     : PolyORB.Services.Naming.NamingContext.Ref;
--        How_Many : PolyORB.Types.Unsigned_Long;
--        Bl       :    out BindingList;
--        Bi       :    out BindingIterator_Forward.Ref);

   List_Repository_Id : constant Standard.String
     := "IDL:omg.org/CosNaming/NamingContext/list:1.0";

   Repository_Id : constant Standard.String
     := "IDL:omg.org/CosNaming/NamingContext:1.0";

end PolyORB.Services.Naming.NamingContext.Client;
