------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . S E R V I C E S . N A M I N G               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 1999-2002 Free Software Fundation              --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  The PolyORB Naming Service is an adaptation from OMG COS Naming, v 1.0

--  $Id$

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
