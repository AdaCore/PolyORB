------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . E X C E P T I O N S                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2015, Free Software Foundation, Inc.          --
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

--  Exceptions management subsystem

--  PolyORB distinguishes errors and exceptions:
--
--  A non null error means that a wrong execution occurs within
--  PolyORB's core middleware or one of its personalities.
--
--  An exception is one possible result of the execution of a
--  personality-specific function or procedure. It is either raised
--  within application personality context, or returned in the request
--  response message.
--
--  When raised, exception is built from error information,
--  translated to personality specific context.

--  PolyORB's core middleware should not raise exceptions, except Ada
--  standard exceptions as defined in the Ada Reference Manual. It
--  should instead return a non null Error_Container.

with Ada.Exceptions;

with PolyORB.Any;
with PolyORB.Errors;
with PolyORB.Types;

package PolyORB.Exceptions is

   use PolyORB.Errors;

   ---------------------
   -- User exceptions --
   ---------------------

   procedure User_Get_Members
     (Occurrence : Ada.Exceptions.Exception_Occurrence;
      Members    : out Exception_Members'Class);
   --  Extract members from User exception occurence

   procedure User_Purge_Members
     (Occurrence : Ada.Exceptions.Exception_Occurrence);
   --  Forget exception members associated with an exception occurrence

   procedure User_Raise_Exception
     (Id      : Ada.Exceptions.Exception_Id;
      Members : Exception_Members'Class;
      Message : Standard.String := "");
   pragma No_Return (User_Raise_Exception);
   --  Raise a user exception with the specified members.

   procedure Raise_User_Exception_From_Any
     (Repository_Id : PolyORB.Types.RepositoryId;
      Occurence     : PolyORB.Any.Any;
      Message       : Standard.String := "");

   type Raise_From_Any_Procedure is
     access procedure (Occurrence : PolyORB.Any.Any;
                       Message    : Standard.String);

   procedure Default_Raise_From_Any (Occurrence : PolyORB.Any.Any);

   procedure Register_Exception
     (TC     : PolyORB.Any.TypeCode.Local_Ref;
      Raiser : Raise_From_Any_Procedure);
   --  Associate the TypeCode for a user-defined exception with
   --  a procedure that raises an occurrence of that exception,
   --  given an Any with that TypeCode.
   --  (When a client creates a request, it is his responsability
   --  to provide the list of typecodes of potential exceptions,
   --  so the generic middleware can unmarshall occurrences and
   --  store them into an Any. It is then the responsibility of
   --  the application layer -- eg. the CORBA PortableServer --
   --  to map the Any back to whatever representation is relevant
   --  in the application personality: here, raising a language
   --  exception with proper members.

   ---------------------------------
   -- Exception utility functions --
   ---------------------------------

   function Exception_Name
     (Repository_Id : Standard.String) return Standard.String;
   --  Return the name of an exception from its repository id

   function Exception_TC
     (Repository_Id : Standard.String) return Any.TypeCode.Local_Ref;
   --  Return the TypeCode of an exception from its repository id

   procedure Exception_Name_To_Error_Id
     (Name     :     String;
      Is_Error : out Boolean;
      Id       : out Error_Id);
   --  Convert an exception name into a PolyORB's Error Id

   function Get_ExcepId_By_Name (Name : Standard.String)
     return Ada.Exceptions.Exception_Id;
   --  Returns the exception id from its name

   function Occurrence_To_Name
     (Occurrence : Ada.Exceptions.Exception_Occurrence) return String;

end PolyORB.Exceptions;
