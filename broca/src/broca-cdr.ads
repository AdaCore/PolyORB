------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                            B R O C A . C D R                             --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
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

with CORBA;
with Broca.Buffers;
use Broca.Buffers;

package Broca.CDR is

   pragma Elaborate_Body;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Octet);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Octet);

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Octet;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Char);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Char);

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Char;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Wchar);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Wchar);

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Wchar;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Boolean);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Boolean);

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Boolean;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Unsigned_Short);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Unsigned_Short);

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Unsigned_Short;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Unsigned_Long);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Unsigned_Long);

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Unsigned_Long;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Unsigned_Long_Long);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Unsigned_Long_Long);

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Unsigned_Long_Long;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Short);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Short);

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Short;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Long);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Long);

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Long;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Long_Long);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Long_Long);

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Long_Long;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Float);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Float);

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Float;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Double);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Double);

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Double;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Long_Double);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Long_Double);

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Long_Double;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.String);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.String);

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.String;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Wide_String);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Wide_String);

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Wide_String;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.Any);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.Any);

   function Unmarshall (Buffer : access Buffer_Type)
     return CORBA.Any;

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access CORBA.NamedValue);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in CORBA.NamedValue);

   procedure Unmarshall
     (Buffer : access Buffer_Type;
      Data : in out CORBA.NamedValue);

   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : access Encapsulation);
   procedure Marshall
     (Buffer : access Buffer_Type;
      Data   : in Encapsulation);

   function Unmarshall (Buffer : access Buffer_Type)
     return Encapsulation;

   procedure Start_Encapsulation
     (Buffer : access Buffer_Type);
   --  Prepare Buffer to receive marshalled data
   --  that will be turned into an Encapsulation.

end Broca.CDR;
