------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.DSA_P.NAME_SERVICE.MDNS.SERVANT                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2010-2012, Free Software Foundation, Inc.          --
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

pragma Ada_2012;

--  This package implements the actual mDNS servant. It stores a local list
--  of entries representing the respective RCI/SP packages and upon reception
--  of a request, looks up the requested package's informations and send them
--  back to the client partition.

with PolyORB.DSA_P.Name_Service.mDNS.Helper;
with PolyORB.Dynamic_Dict;
with PolyORB.Minimal_Servant;
with PolyORB.Requests;
with PolyORB.Types;

package PolyORB.DSA_P.Name_Service.mDNS.Servant is

   use PolyORB.DSA_P.Name_Service.mDNS.Helper;

   type Object is new PolyORB.Minimal_Servant.Servant with null record;
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

   procedure Query (Self : access Object;
                    Authoritative : in out Boolean;
                    Question : rrSequence;
                    Answer : out rrSequence;
                    Authority : out rrSequence;
                    Additional : out rrSequence;
                    Response : out Rcode);
   --  This procedure is called by Invoke upon reception of distant query
   --  Depending on the incoming request, it generates the corresponding
   --  Resource Records by lookup up the local list of Local_Entry objects.

   overriding procedure Invoke
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
                             Additional_Seq : out rrSequence;
                             Response       : out Rcode);
   --  The local procedure responsible for looking up for the record and
   --  constructing the answer sequences conforming to the RCI/SP<->RR mapping

   procedure Parse_Question_Name (Question : PolyORB.Types.String;
                                  Name : out PolyORB.Types.String;
                                  Kind : out PolyORB.Types.String);
   --  Extract the Name and Kind of the requested package from the incoming
   --  Question name
end PolyORB.DSA_P.Name_Service.mDNS.Servant;
