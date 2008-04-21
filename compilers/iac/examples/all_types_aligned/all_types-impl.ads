------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       A L L _ T Y P E S . I M P L                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2008, Free Software Foundation, Inc.          --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with CORBA;
--  with CORBA.Object;
with PortableServer;

package all_types.Impl is
   type Object is new PortableServer.Servant_Base with null record;

   --  This is simply used to define the operations.

   function echoBoolean
     (Self : access Object;
      arg : CORBA.Boolean) return CORBA.Boolean;

   function echoShort
     (Self : access Object;
      arg : CORBA.Short) return CORBA.Short;

   function echoLong
     (Self : access Object;
      arg : CORBA.Long) return CORBA.Long;

   function echoUShort
     (Self : access Object;
      arg : CORBA.Unsigned_Short) return CORBA.Unsigned_Short;

   function echoULong
     (Self : access Object;
      arg : CORBA.Unsigned_Long) return CORBA.Unsigned_Long;

   function echoULLong
     (Self : access Object;
      arg : CORBA.Unsigned_Long_Long) return CORBA.Unsigned_Long_Long;

   function echoFloat
     (Self : access Object;
      arg : CORBA.Float) return CORBA.Float;

   function echoDouble
     (Self : access Object;
      arg : CORBA.Double) return CORBA.Double;

   function echoChar
     (Self : access Object;
      arg : CORBA.Char) return CORBA.Char;

   function echoWChar
     (Self : access Object;
      arg : CORBA.Wchar) return CORBA.Wchar;

   function echoOctet
     (Self : access Object;
      arg : CORBA.Octet) return CORBA.Octet;

   function EchoBooleanAndLong
     (Self : access Object;
      arg1 : CORBA.Boolean;
      arg2 : CORBA.Long) return CORBA.Long;

   function EchoBooleanAndShort
     (Self : access Object;
      arg1 : CORBA.Boolean;
      arg2 : CORBA.Short) return CORBA.Short;

   function echoString
     (Self : access Object;
      arg : CORBA.String) return CORBA.String;

   function echoWString
     (Self : access Object;
      arg : CORBA.Wide_String) return CORBA.Wide_String;

   function echoUnion
     (Self : access Object;
      arg : Simple_Union) return Simple_Union;

   function echoUnionAndString
     (Self : access Object;
      arg1 : Simple_Union;
      arg2 : CORBA.string) return Simple_Union;

   function echoStringAndUnion
     (Self : access Object;
      arg1 : Simple_Union;
      arg2 : CORBA.string) return CORBA.String;

   function echoStruct
     (Self : access Object;
      arg : Simple_Struct) return Simple_Struct;

   function echoUsequence
     (Self : access Object;
      arg  : U_sequence) return U_sequence;

   function echoBsequence
     (Self : access Object;
      arg  : B_sequence) return B_Sequence;

   procedure StopServer (Self : access Object);

end all_types.Impl;
