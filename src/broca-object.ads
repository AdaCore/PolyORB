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
with Broca.IOP;

package Broca.Object is

   pragma Elaborate_Body;

   type Object_Type (Local_Object : Boolean) is
     new CORBA.Impl.Object with private;

   type Object_Ptr is access all Object_Type'Class;

   procedure Finalize
     (X : in out Object_Type);

   procedure Marshall
     (Buffer : access Broca.Buffers.Buffer_Type;
      Value  : in Object_Type);

   procedure Unmarshall
     (Buffer : access Broca.Buffers.Buffer_Type;
      Result : out Broca.Object.Object_Type);

   function Find_Profile
     (Object : in Object_Ptr)
     return IOP.Profile_Ptr;
   --  Find a profile for a message

   function Get_Type_Id (Object : in Object_Type) return CORBA.String;
   pragma Inline (Get_Type_Id);

   function Create_Object_From_IOR
     (IOR : access Broca.Buffers.Buffer_Type)
     return Object_Ptr;

   --  It is used by Broca.ORB.Build_Remote_Naming_Reference
   --  It will be nice if this will be not used at all
--     function Create_Custom_Object
--       (Type_Id : in CORBA.String;
--        Host : in CORBA.String;
--        Port : in CORBA.Unsigned_Short;
--        Object_Key : in Broca.Sequences.Octet_Sequence)
--        return Object_Ptr;

   --  Creates object
   function Create_Object
     (Type_Id : in CORBA.String;
      Profiles : Broca.IOP.Profile_Ptr_Array_Ptr;
      Local_Object : in Boolean)
     return Object_Ptr;

private

   type Object_Type (Local_Object : Boolean) is
     new CORBA.Impl.Object with
      record
         Type_Id  : CORBA.String;
         Profiles : Broca.IOP.Profile_Ptr_Array_Ptr := null;
         case Local_Object is
            when False =>
               Used_Profile_Index   : CORBA.Unsigned_Long := 0;
               Is_Supported_Profile : Boolean             := False;
               --  True if server is placed in the same address space with
               --  Object.
            when True =>
               null;
         end case;
      end record;

end Broca.Object;

