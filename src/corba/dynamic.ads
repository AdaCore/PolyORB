------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              D Y N A M I C                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with CORBA.Sequences.Unbounded;
pragma Elaborate_All (CORBA.Sequences.Unbounded);

package Dynamic is

   type Parameter is record
      Argument : CORBA.Any;
      --      Mode     : CORBA.ParameterMode;
   end record;

   package IDL_SEQUENCE_Dynamic_Parameter is
      new CORBA.Sequences.Unbounded (Dynamic.Parameter);

   type ParameterList is
      new IDL_SEQUENCE_Dynamic_Parameter.Sequence;

   --  XXX This type temporary replace CORBA::StringSeq type.
   package IDL_Sequence_String is new CORBA.Sequences.Unbounded (CORBA.String);

   type ContextList is new IDL_Sequence_String.Sequence;

   package IDL_SEQUENCE_CORBA_TypeCode is
      new CORBA.Sequences.Unbounded (CORBA.TypeCode.Object);

   type ExceptionList is
      new Dynamic.IDL_SEQUENCE_CORBA_TypeCode.Sequence;

   type RequestContext is
      new IDL_Sequence_String.Sequence;

   --  Repository_Ids

   Repository_Id : constant Standard.String
     := "IDL:omg.org/Dynamic:1.0";

   Parameter_Repository_Id : constant Standard.String
     := "IDL:omg.org/Dynamic/Parameter:1.0";

   ParameterList_Repository_Id : constant Standard.String
     := "IDL:omg.org/Dynamic/ParameterList:1.0";

   ContextList_Repository_Id : constant Standard.String
     := "IDL:omg.org/Dynamic/ContextList:1.0";

   ExceptionList_Repository_Id : constant Standard.String
     := "IDL:omg.org/Dynamic/ExceptionList:1.0";

   RequestContext_Repository_Id : constant Standard.String
     := "IDL:omg.org/Dynamic/RequestContext:1.0";

end Dynamic;
