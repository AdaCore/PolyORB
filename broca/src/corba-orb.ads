------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                            C O R B A . O R B                             --
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

with CORBA; use CORBA;
with CORBA.Object;
with CORBA.Sequences.Unbounded;
pragma Elaborate_All (CORBA.Sequences.Unbounded);

--  with CORBA.BOA;

--  with CORBA.NVList;
--  with CORBA.OperationDef;
--  with CORBA.Context;
--  with CORBA.Sequences;

package CORBA.ORB is

   --  21.34

--    type Octet_Sequence is new CORBA.Sequences.Unbounded (Octet);
--    type ServiceDetail is record
--       Service_Detail_Type: ServiceDetailType;
--       Service_Detail: Octet_Sequence.Sequence;
--    end record;

   type ObjectId is new CORBA.String;
   package IDL_SEQUENCE_ObjectId is new CORBA.Sequences.Unbounded (ObjectId);
   type ObjectIdList is new IDL_SEQUENCE_ObjectId.Sequence;

--    function Object_To_String
--      (Obj : in Broca.Imp_object.Implemented_Object'class)
--       return CORBA.String;

   procedure String_To_Object
     (From : in  CORBA.String;
      To   : out CORBA.Object.Ref'Class);
   --  Returns a Ref'Class out of an IOR it is called by
   --  CORBA.ORB.String_To_Object see CORBA specification for details

   --  Obtaining initial object references.

   function List_Initial_Services return ObjectIdList;

   function Resolve_Initial_References
     (Identifier : ObjectId)
     return CORBA.Object.Ref;

   procedure Run;

   ------------------------
   -- ORB initialization --
   ------------------------

--    function ORB_Init
--      (ORB_Name : in Standard.String)
--       return Object;
--    --  Initializes the ORB with parameters of the command line and returns
--    --  the ORB

--    function BOA_Init
--      (Self     : in Object;
--       BOA_Name : in Standard.String)
--       return CORBA.BOA.Object;
--    --  Initializes the BOA with parameters of the command line and returns
--    --  the BOA

end CORBA.ORB;
