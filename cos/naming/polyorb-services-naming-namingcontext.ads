------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.SERVICES.NAMING.NAMINGCONTEXT                   --
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

--  $Id$

with Ada.Exceptions;

with PolyORB.Exceptions;
with PolyORB.References;

package PolyORB.Services.Naming.NamingContext is

   type Ref is new PolyORB.References.Ref with null record;

   type NotFoundReason is
     (missing_node,
      not_context,
      not_object);

   NotFoundReason_Repository_Id : constant Standard.String
     := "IDL:omg.org/CosNaming/NamingContext/NotFoundReason:1.0";

   type NotFound_Members is
     new PolyORB.Exceptions.Exception_Members with
   record
      why : NotFoundReason;
      rest_of_name : Name;
   end record;

   NotFound : exception;

   NotFound_Repository_Id : constant Standard.String
     := "IDL:omg.org/CosNaming/NamingContext/NotFound:1.0";

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out NotFound_Members);

   type CannotProceed_Members is
     new PolyORB.Exceptions.Exception_Members with
   record
      cxt : Ref;
      rest_of_name : Name;
   end record;

   CannotProceed : exception;

   CannotProceed_Repository_Id : constant Standard.String
     := "IDL:omg.org/CosNaming/NamingContext/CannotProceed:1.0";

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out CannotProceed_Members);

   type InvalidName_Members is
     new PolyORB.Exceptions.Exception_Members with
   null record;

   InvalidName : exception;

   InvalidName_Repository_Id : constant Standard.String
     := "IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0";

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out InvalidName_Members);

   type AlreadyBound_Members is
     new PolyORB.Exceptions.Exception_Members with
   null record;

   AlreadyBound : exception;

   AlreadyBound_Repository_Id : constant Standard.String
     := "IDL:omg.org/CosNaming/NamingContext/AlreadyBound:1.0";

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out AlreadyBound_Members);

   type NotEmpty_Members is
     new PolyORB.Exceptions.Exception_Members with
   null record;

   NotEmpty : exception;

   NotEmpty_Repository_Id : constant Standard.String
     := "IDL:omg.org/CosNaming/NamingContext/NotEmpty:1.0";

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out NotEmpty_Members);


end PolyORB.Services.Naming.NamingContext;
