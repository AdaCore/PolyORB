------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                         B R O C A . O B J E C T                          --
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
with CORBA.Impl;
with Broca.Buffers;
--  with Broca.Refs;
with Broca.IOP;

with CORBA.Impl;

package Broca.Object is

   type Object_Type is new CORBA.Impl.Object with
      record
         Type_Id  : CORBA.String;
         Profiles : IOP.Profile_Ptr_Array_Ptr;
      end record;

   type Object_Ptr is access all Object_Type'Class;

   procedure Marshall
     (Buffer : access Broca.Buffers.Buffer_Type;
      Value  : in Broca.Object.Object_Type);

   procedure Unmarshall
     (Buffer : access Broca.Buffers.Buffer_Type;
      Result : out Broca.Object.Object_Type);

   function Find_Profile
     (Object : Object_Ptr)
     return IOP.Profile_Ptr;
   --  Find a profile for a message

end Broca.Object;
