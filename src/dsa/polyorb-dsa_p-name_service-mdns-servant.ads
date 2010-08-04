------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.DSA_P.NAME_SERVICE.MDNS.SERVANT                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--           Copyright (C) 2010, Free Software Foundation, Inc.             --
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

with PolyORB.Minimal_Servant;
with PolyORB.Requests;
with PolyORB.DSA_P.Name_Service.mDNS.Helper;
with PolyORB.Dynamic_Dict;

package PolyORB.DSA_P.Name_Service.mDNS.Servant is
   use PolyORB.DSA_P.Name_Service.mDNS.Helper;

   type Object is
     new PolyORB.Minimal_Servant.Servant with null record;
   type Object_Ptr is access all Object'Class;
   --  The actual servant object

   type Local_Entry is record
      Name : PolyORB.Types.String;
      --  Name of the package

      Kind : PolyORB.Types.String;
      --  Kind of the package

      Version : PolyORB.Types.String;
      --  Version of the package

      Base_Ref : PolyORB.References.Ref;
      --  Actual reference to the package
   end record;
   type Local_Entry_Ptr is access all Local_Entry;
   --  A local entry contains all the data relevant to packages that we need

   package Local_Entry_List is new PolyORB.Dynamic_Dict (Local_Entry_Ptr);
   --  The list of all entries. key=Unit Name, value - pointer to Local_Entry

   procedure Query (Self : access Object; authoritative : in out Boolean;
                    question : rrSequence;
                    answer : out rrSequence;
                    authority : out rrSequence;
                    additional : out rrSequence;
                    Response : out Rcode);
   --  This procedure is called Invoke upon reception of distant query
   --  Depending on the incoming request, it generates the corresponding
   --  Resource Records by lookup up the local list of Local_Entry objects.

   procedure Invoke
     (Self     : access Object;
      Request  : PolyORB.Requests.Request_Access);
   --  Overriding the abstract servant's Invoke procedure. Used to create
   --  an empty argument's list, populate it from Request.Args, invoke Query
   --  and assign back the out arguments.

   procedure Append_Entry_To_Context
                          (Name : PolyORB.Types.String;
                           Kind : PolyORB.Types.String;
                           Version : PolyORB.Types.String;
                           Base_Ref : PolyORB.References.Ref);
   --  Creates a new Local_Entry from the in data provided and appends it
   --  to the Local_Entry_List

private
   procedure Find_Answer_RR (Question       : RR;
                             Answer_Seq     : out rrSequence;
                             Authority_Seq  : out rrSequence;
                             Additional_Seq : out rrSequence);
   --  The local procedure responsible for looking up for the record and
   --  constructing the answer sequences conforming to the RCI/SP<->RR mapping
end PolyORB.DSA_P.Name_Service.mDNS.Servant;
