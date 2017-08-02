------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . G I O P _ P . S E R V I C E _ C O N T E X T S       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2017, Free Software Foundation, Inc.          --
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

with PolyORB.Log;
with PolyORB.Representations.CDR.Common;
with PolyORB.Types;

package body PolyORB.GIOP_P.Service_Contexts is

   use PolyORB.Log;
   use PolyORB.Representations.CDR.Common;
   use PolyORB.QoS.Service_Contexts;
   use PolyORB.Types;

   package L is
      new PolyORB.Log.Facility_Log ("polyorb.giop_p.service_contexts");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   -----------------------------------
   -- Marshall_Service_Context_List --
   -----------------------------------

   procedure Marshall_Service_Context_List
     (Buffer : access Buffers.Buffer_Type;
      SCP    : PRSC.QoS_GIOP_Service_Contexts_Parameter_Access)
   is
      use PolyORB.QoS.Service_Contexts.Service_Context_Lists;

      Iter : Iterator;
   begin
      if SCP = null then
         Marshall (Buffer, Types.Unsigned_Long'(0));
         return;
      end if;

      pragma Debug (C, O ("Marshall_Service_Context_List: enter, length="
                       & Integer'Image (Length (SCP.Service_Contexts))));

      Iter := First (SCP.Service_Contexts);

      Marshall (Buffer, Types.Unsigned_Long (Length (SCP.Service_Contexts)));

      while not Last (Iter) loop
         Marshall (Buffer, Unsigned_Long (Value (Iter).Context_Id));
         Marshall (Buffer, Value (Iter).Context_Data.all);
         Next (Iter);
      end loop;

      pragma Debug (C, O ("Marshall_Service_Context_List: leave"));
   end Marshall_Service_Context_List;

   -------------------------------------
   -- Unmarshall_Service_Context_List --
   -------------------------------------

   procedure Unmarshall_Service_Context_List
     (Buffer : access Buffers.Buffer_Type;
      SCP    :    out PRSC.QoS_GIOP_Service_Contexts_Parameter_Access)
   is
      use Service_Context_Lists;

      Length : constant PolyORB.Types.Unsigned_Long := Unmarshall (Buffer);

   begin
      pragma Debug (C, O ("Unmarshall_Service_Context_List: enter, length ="
                       & PolyORB.Types.Unsigned_Long'Image (Length)));

      if Length = 0 then
         SCP := null;
         return;
      end if;

      SCP := new QoS_GIOP_Service_Contexts_Parameter;

      for J in 1 .. Length loop
         Append
           (SCP.Service_Contexts,
            (Types.Unsigned_Long'(Unmarshall (Buffer)),
             new Encapsulation'(Unmarshall (Buffer))));
      end loop;

      pragma Debug (C, O ("Unmarshall_Service_Context_List: leave"));
   end Unmarshall_Service_Context_List;

end PolyORB.GIOP_P.Service_Contexts;
