------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--          A D A B R O K E R . N E T B U F F E R E D S T R E A M           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.4 $
--                                                                          --
--         Copyright (C) 1999-2000 ENST Paris University, France.           --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  This unit is wrapped around a C++ class whose name is
--  Ada_netBufferedStream. (see Ada_netBufferedStream.hh) It provides two
--  types of methods : the C functions of the Ada_netBufferedStream class
--  and their equivalent in Ada. (he first ones have a C_ prefix.)  In
--  addition, there is a raise_ada_exception function that allows C
--  functions to raise the ada No_Initialisation exception.  At last, there
--  is only one Init procedure in place of two in Ada_netBufferedStream
--  since the second one is useless for AdaBroker.

with Ada.Unchecked_Deallocation;

with Interfaces.CPP;

with System;

with CORBA;

with AdaBroker; use AdaBroker;
with AdaBroker.Sysdep;
with AdaBroker.Rope;

package AdaBroker.NetBufferedStream is

   type Object is tagged record
      C_Object : System.Address   := System.Null_Address;
      Init_Ok  : Sysdep.Bool := Sysdep.False;
      Table    : Interfaces.CPP.Vtable_Ptr;
   end record;
   --  C_Object       : pointer on the underlying C netBufferedStream object
   --  Init_Ok  (C)   : state of the object (initialized or not)
   --  Table    (Ada) : needed to interface C++ and Ada

   pragma Cpp_Class (Object);
   pragma Cpp_Vtable (Object, Table, 1);
   --  This type is both a C and an Ada class it is wrapped around
   --  Ada_netBufferedStream (see Ada_netBufferedStream.hh)

   type Object_Ptr is access all Object;
   --  Type pointer on type Object

   procedure Free is new Ada.Unchecked_Deallocation (Object, Object_Ptr);
   --  To deallocate Object_Ptr

   procedure Init
     (Self    : in Object'Class;
      R       : in Rope.Object;
      RdLock  : in CORBA.Boolean;
      WrLock  : in CORBA.Boolean;
      BufSize : in CORBA.Unsigned_Long);
   --  Ada constructor of the class.  This function must be called after
   --  each declaration of an Object object. If it is not, you can not use
   --  the object.

   procedure Marshall
     (A : in CORBA.Char;
      S : in out Object'Class);
   --  Marshalls a CORBA.Char into a netbufferedstream object

   procedure Unmarshall
     (A : out CORBA.Char;
      S : in out Object'Class);
   --  Unmarshalls a CORBA.Char from a netbufferedstream object

   function Align_Size
     (A              : in CORBA.Char;
      Initial_Offset : in CORBA.Unsigned_Long;
      N              : in CORBA.Unsigned_Long := 1)
      return CORBA.Unsigned_Long;
   --  Compute the size needed to marshall A into S

   procedure Marshall
     (A : in CORBA.Boolean;
      S : in out Object'Class);
   --  Marshalls a CORBA.Boolean into a netbufferedstream object

   procedure Unmarshall
     (A : out CORBA.Boolean;
      S : in out Object'Class);
   --  Unmarshalls a CORBA.Boolean from a netbufferedstream object

   function Align_Size
     (A              : in CORBA.Boolean;
      Initial_Offset : in CORBA.Unsigned_Long;
      N              : in CORBA.Unsigned_Long := 1)
      return CORBA.Unsigned_Long;
   --  Compute the size needed to marshall A into S

   procedure Marshall
     (A : in CORBA.Short;
      S : in out Object'Class);
   --  Marshalls a CORBA.Short into a netbufferedstream object

   procedure Unmarshall
     (A : out CORBA.Short;
      S : in out Object'Class);
   --  Unmarshalls a CORBA.Short from a netbufferedstream object


   function Align_Size
     (A              : in CORBA.Short;
      Initial_Offset : in CORBA.Unsigned_Long;
      N              : in CORBA.Unsigned_Long := 1)
      return CORBA.Unsigned_Long;
   --  Compute the size needed to marshall A into S


   procedure Marshall
     (A : in CORBA.Unsigned_Short;
      S : in out Object'Class);
   --  Marshalls a CORBA.Unsigned_Short into a netbufferedstream object

   procedure Unmarshall
     (A : out CORBA.Unsigned_Short;
      S : in out Object'Class);
   --  Unmarshalls a CORBA.Unsigned_Short from a netbufferedstream object

   function Align_Size
     (A              : in CORBA.Unsigned_Short;
      Initial_Offset : in CORBA.Unsigned_Long;
      N              : in CORBA.Unsigned_Long := 1)
      return CORBA.Unsigned_Long;
   --  Compute the size needed to marshall A into S

   procedure Marshall
     (A : in CORBA.Long;
      S : in out Object'Class);
   --  Marshalls a CORBA.Long into a netbufferedstream object

   procedure Unmarshall
     (A : out CORBA.Long;
      S : in out Object'Class);
   --  Unmarshalls a CORBA.Long from a netbufferedstream object

   function Align_Size
     (A              : in CORBA.Long;
      Initial_Offset : in CORBA.Unsigned_Long;
      N              : in CORBA.Unsigned_Long := 1)
      return CORBA.Unsigned_Long;
   --  Compute the size needed to marshall A into S

   procedure Marshall
     (A : in CORBA.Unsigned_Long;
      S : in out Object'Class);
   --  Marshalls a CORBA.Unsigned_Long into a netbufferedstream object

   procedure Unmarshall
     (A : out CORBA.Unsigned_Long;
      S : in out Object'Class);
   --  Unmarshalls a CORBA.Unsigned_Long from a netbufferedstream object

   function Align_Size
     (A              : in CORBA.Unsigned_Long;
      Initial_Offset : in CORBA.Unsigned_Long;
      N              : in CORBA.Unsigned_Long := 1)
      return CORBA.Unsigned_Long;
   --  Compute the size needed to marshall A into S

   procedure Marshall
     (A : in CORBA.Float;
      S : in out Object'Class);
   --  Marshalls a CORBA.Float into a netbufferedstream object

   procedure Unmarshall
     (A : out CORBA.Float;
      S : in out Object'Class);
   --  Unmarshalls a CORBA.Float from a netbufferedstream object

   function Align_Size
     (A              : in CORBA.Float;
      Initial_Offset : in CORBA.Unsigned_Long;
      N              : in CORBA.Unsigned_Long := 1)
                        return CORBA.Unsigned_Long;
   --  Compute the size needed to marshall A into S

   procedure Marshall
     (A : in CORBA.Double;
      S : in out Object'Class);
   --  Marshalls a CORBA.Double into a netbufferedstream object

   procedure Unmarshall
     (A : out CORBA.Double;
      S : in out Object'Class);
   --  Unmarshalls a CORBA.Double from a netbufferedstream object

   function Align_Size
     (A              : in CORBA.Double;
      Initial_Offset : in CORBA.Unsigned_Long;
      N              : in CORBA.Unsigned_Long := 1)
      return CORBA.Unsigned_Long;
   --  Compute the size needed to marshall A into S

   procedure Marshall
     (A : in CORBA.Octet;
      S : in out Object'Class);
   --  Marshalls a CORBA.Octet into a netbufferedstream object

   procedure Unmarshall
     (A : out CORBA.Octet;
      S : in out Object'Class);
   --  Unmarshalls a CORBA.Octet from a netbufferedstream object

   function Align_Size
     (A              : in CORBA.Octet;
      Initial_Offset : in CORBA.Unsigned_Long;
      N              : in CORBA.Unsigned_Long := 1)
                        return CORBA.Unsigned_Long;
   --  Compute the size needed to marshall A into S

   procedure Marshall
     (A : in CORBA.String;
      S : in out Object'Class);
   --  Marshalls a CORBA.String into a netbufferedstream object

   procedure Unmarshall
     (A : out CORBA.String;
      S : in out Object'Class);
   --  Unmarshalls a CORBA.String from a netbufferedstream object

   function Align_Size
     (A              : in CORBA.String;
      Initial_Offset : in CORBA.Unsigned_Long;
      N              : in CORBA.Unsigned_Long := 1)
      return CORBA.Unsigned_Long;
   --  Compute the size needed to marshall A into S

   procedure Marshall
     (A : in CORBA.Completion_Status;
      S : in out Object'Class);
   --  Marshalls a CORBA.Completion_Status into a netbufferedstream object

   procedure Unmarshall
     (A : out CORBA.Completion_Status;
      S : in out Object'Class);
   --  Unmarshalls a CORBA.Completion_Status from a netbufferedstream object

   function Align_Size
     (A              : in CORBA.Completion_Status;
      Initial_Offset : in CORBA.Unsigned_Long;
      N              : in CORBA.Unsigned_Long := 1)
      return CORBA.Unsigned_Long;
   --  Compute the size needed to marshall A into S

   procedure Marshall
     (A : in CORBA.Ex_Body'Class;
      S : in out Object'Class);
   --  Marshalls a CORBA system exception into a netbufferedstream object

   procedure Unmarshall
     (A : out CORBA.Ex_Body'Class;
      S : in out Object'Class);
   --  Unmarshalls a CORBA system exception from a netbufferedstream object

   function Align_Size
     (A              : in CORBA.Ex_Body'Class;
      Initial_Offset : in CORBA.Unsigned_Long;
      N              : in CORBA.Unsigned_Long := 1)
      return CORBA.Unsigned_Long;
   --  Compute the size needed to marshall A into S

   function Is_Reusing_Existing_Connection
     (Self : in Object'Class)
      return CORBA.Boolean;
   --  Indicates whether the netbufferedstream is reusing an existing
   --  connection or not.  (see rope.h L400 for more details)

private

   function Constructor return Object'Class;
   pragma Cpp_Constructor (Constructor);
   pragma Import (CPP, Constructor, "__21Ada_netBufferedStream");
   --  Default constructor of the C class.  Actually, this constructor does
   --  nothing and you must call Init to init properly an object.

end AdaBroker.NetBufferedStream;
