------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.DSA_P.NAME_SERVICE.MDNS.CLIENT                   --
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

--  This package implements the mDNS request invocation procedure, when a
--  partition is looking up a remote package's informations

with PolyORB.DSA_P.Name_Service.mDNS.Helper;
with PolyORB.References;
with PolyORB.Types;

package PolyORB.DSA_P.Name_Service.mDNS.Client is

   function Resolve
     (The_Ref : PolyORB.References.Ref;
      Name    : String;
      Kind    : String)
      return PolyORB.References.Ref;
   --  The Resolve function is responsible for construction a Question RR from
   --  a Name and Kind of a package and invoking the Query procedure. Upon
   --  reception of the result, it constructs a PolyORB.References.Ref
   --  representing the remote reference and returns it to the
   --  Nameserver_Lookup function.

   procedure Query
     (Self : PolyORB.References.Ref;
      Authoritative : in out PolyORB.Types.Boolean;
      Question : PolyORB.DSA_P.Name_Service.mDNS.Helper.rrSequence;
      Answer : out PolyORB.DSA_P.Name_Service.mDNS.Helper.rrSequence;
      Authority : out PolyORB.DSA_P.Name_Service.mDNS.Helper.rrSequence;
      Additional : out PolyORB.DSA_P.Name_Service.mDNS.Helper.rrSequence;
      Returns : out PolyORB.DSA_P.Name_Service.mDNS.Helper.Rcode);
   --  The Query procedure is responsible for contructing a request from the
   --  IN Question argument and invoking it. It retrieves the OUT results and
   --  stores them in the Answer/Authority/Additional rr sequences, as well as
   --  the DNS Rcode associated.

private
   procedure Parse_TXT_Record (Answer_RR  : PolyORB.Types.String;
                               Str_Ref    : out PolyORB.Types.String;
                               Version_id : out PolyORB.Types.String);
   --  Extract the stringified reference and the version of the package
   --  from the TXT record.
end PolyORB.DSA_P.Name_Service.mDNS.Client;
