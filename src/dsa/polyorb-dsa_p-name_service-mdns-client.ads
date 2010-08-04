------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.DSA_P.NAME_SERVICE.MDNS.CLIENT                   --
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

with PolyORB.References;
with PolyORB.DSA_P.Name_Service.mDNS.Helper;

package PolyORB.DSA_P.Name_Service.mDNS.Client is

   function Resolve
     (The_Ref : PolyORB.References.Ref;
      Name    : String;
      Kind    : String)
      return PolyORB.References.Ref;

   procedure Query
     (Self : PolyORB.References.Ref;
      authoritative : in out PolyORB.Types.Boolean;
      question : PolyORB.DSA_P.Name_Service.mDNS.Helper.rrSequence;
      answer : out PolyORB.DSA_P.Name_Service.mDNS.Helper.rrSequence;
      authority : out PolyORB.DSA_P.Name_Service.mDNS.Helper.rrSequence;
      additional : out PolyORB.DSA_P.Name_Service.mDNS.Helper.rrSequence;
      Returns : out PolyORB.DSA_P.Name_Service.mDNS.Helper.Rcode);

private
   procedure Parse_TXT_Record (Answer_RR : PolyORB.Types.String;
                               Version_id : out PolyORB.Types.String);

end PolyORB.DSA_P.Name_Service.mDNS.Client;
