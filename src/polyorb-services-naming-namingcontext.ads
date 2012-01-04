------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.SERVICES.NAMING.NAMINGCONTEXT                   --
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

with Ada.Exceptions;

with PolyORB.Errors;
with PolyORB.References;

package PolyORB.Services.Naming.NamingContext is

   type Ref is new PolyORB.References.Ref with null record;
   Nil_Ref : constant Ref := (References.Nil_Ref with null record);

   type NotFoundReason is
     (missing_node,
      not_context,
      not_object);

   NotFoundReason_Repository_Id : constant Standard.String
     := "IDL:omg.org/CosNaming/NamingContext/NotFoundReason:1.0";

   type NotFound_Members is
     new PolyORB.Errors.Exception_Members with
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
     new PolyORB.Errors.Exception_Members with
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
     new PolyORB.Errors.Exception_Members with
   null record;

   InvalidName : exception;

   InvalidName_Repository_Id : constant Standard.String
     := "IDL:omg.org/CosNaming/NamingContext/InvalidName:1.0";

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out InvalidName_Members);

   type AlreadyBound_Members is
     new PolyORB.Errors.Exception_Members with
   null record;

   AlreadyBound : exception;

   AlreadyBound_Repository_Id : constant Standard.String
     := "IDL:omg.org/CosNaming/NamingContext/AlreadyBound:1.0";

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out AlreadyBound_Members);

   type NotEmpty_Members is
     new PolyORB.Errors.Exception_Members with
   null record;

   NotEmpty : exception;

   NotEmpty_Repository_Id : constant Standard.String
     := "IDL:omg.org/CosNaming/NamingContext/NotEmpty:1.0";

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out NotEmpty_Members);

end PolyORB.Services.Naming.NamingContext;
