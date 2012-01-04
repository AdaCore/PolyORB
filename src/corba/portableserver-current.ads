------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               P O R T A B L E S E R V E R . C U R R E N T                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This specification is derived from the CORBA Specification, and adapted  --
-- for use with PolyORB. The copyright notice above, and the license        --
-- provisions that follow apply solely to the contents neither explicitly   --
-- nor implicitly specified by the CORBA Specification defined by the OMG.  --
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

with Ada.Exceptions;

with CORBA.Current;
with CORBA.Local;
with CORBA.Object;

package PortableServer.Current is

   type Local_Ref is new CORBA.Current.Local_Ref with private;

   function To_Ref (Self : CORBA.Object.Ref'Class) return Local_Ref;

   NoContext : exception;

   function Get_POA (Self : Local_Ref) return PortableServer.POA_Forward.Ref;
   function Get_Object_Id (Self : Local_Ref) return ObjectId;
   function Get_Reference (Self : Local_Ref) return CORBA.Object.Ref;
   function Get_Servant (Self : Local_Ref) return Servant;

   ---------------------------------------
   -- PortableServer.Current exceptions --
   ---------------------------------------

   --  NoContext_Members

   type NoContext_Members is new CORBA.IDL_Exception_Members
     with null record;

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out NoContext_Members);

   Repository_Id : constant Standard.String
     := "IDL:omg.org/PortableServer/Current:1.0";

private

   type Local_Ref is new CORBA.Current.Local_Ref with null record;

   type Current_Object is new CORBA.Local.Object with null record;

   function Is_A
     (Obj             : not null access Current_Object;
      Logical_Type_Id : Standard.String) return Boolean;

end PortableServer.Current;
