------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . S E R V I C E S . N A M I N G               --
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

--  The PolyORB Naming Service is an adaptation from OMG COS Naming, v 1.0

--  with CORBA;
--  with CORBA.Forward;
--  pragma Elaborate_All (CORBA.Forward);

with PolyORB.Sequences.Unbounded;
pragma Elaborate_All (PolyORB.Sequences.Unbounded);

with PolyORB.Types;

package PolyORB.Services.Naming is

   type Istring is new PolyORB.Types.String;

   Istring_Repository_Id : constant Standard.String
     := "IDL:omg.org/CosNaming/Istring:1.0";

   type NameComponent is record
      id : Istring;
      kind : Istring;
   end record;

   NameComponent_Repository_Id : constant Standard.String
     := "IDL:omg.org/CosNaming/NameComponent:1.0";

   package SEQUENCE_NameComponent is
     new PolyORB.Sequences.Unbounded (NameComponent);

   type Name is new SEQUENCE_NameComponent.Sequence;

   Name_Repository_Id : constant Standard.String
     := "IDL:omg.org/CosNaming/Name:1.0";

   type BindingType is
     (Nobject,
      Ncontext);

   BindingType_Repository_Id : constant Standard.String
     := "IDL:omg.org/CosNaming/BindingType:1.0";

   type Binding is record
      binding_name : Name;
      binding_type : BindingType;
   end record;

   Binding_Repository_Id : constant Standard.String
     := "IDL:omg.org/CosNaming/Binding:1.0";

   package SEQUENCE_Binding is
     new PolyORB.Sequences.Unbounded (Binding);

   type BindingList is new SEQUENCE_Binding.Sequence;

   BindingList_Repository_Id : constant Standard.String
     := "IDL:omg.org/CosNaming/BindingList:1.0";

   --  package BindingIterator_Forward is new CORBA.Forward;

end PolyORB.Services.Naming;
