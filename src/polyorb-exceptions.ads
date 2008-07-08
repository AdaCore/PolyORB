------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O L Y O R B . E X C E P T I O N S                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2007, Free Software Foundation, Inc.          --
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

   function Exception_Name (Repository_Id : Standard.String)
     return Standard.String;
   --  Return the name of an exception from its repository ID

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
