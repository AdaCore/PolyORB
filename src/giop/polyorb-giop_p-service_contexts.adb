------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--      P O L Y O R B . G I O P _ P . S E R V I C E _ C O N T E X T S       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2004 Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with PolyORB.Log;
with PolyORB.Representations.CDR.Common;
with PolyORB.Tasking.Priorities;

package body PolyORB.GIOP_P.Service_Contexts is

   use PolyORB.Buffers;
   use PolyORB.Log;
   use PolyORB.Representations.CDR.Common;
   use PolyORB.Types;

   package L is
      new PolyORB.Log.Facility_Log ("polyorb.giop_p.service_contexts");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   -----------------------------------
   -- Marshall_Service_Context_List --
   -----------------------------------

   procedure Marshall_Service_Context_List
     (Buffer : access Buffers.Buffer_Type;
      QoS    : in     PolyORB.Request_QoS.QoS_Parameter_Lists.List;
      CS     : in     Code_Set_Context_Access)
   is
      use PolyORB.Request_QoS;
      use PolyORB.Request_QoS.QoS_Parameter_Lists;

      It : Iterator := First (QoS);
      SC_Length : Natural := Length (QoS);

   begin
      pragma Debug (O ("Marshall_Service_Context_List: enter, length="
                       & Integer'Image (Length (QoS))));

      if CS /= null then
         SC_Length := SC_Length + 1;
      end if;

      Marshall (Buffer, Types.Unsigned_Long (SC_Length));

      if CS /= null then
         declare
            Temp_Buf : Buffer_Access := new Buffer_Type;

         begin
            --  Marshalling a CodeSets service context

            Marshall (Buffer, CodeSets);

            pragma Debug (O ("Processing CodeSets service context"));

            pragma Debug (O ("TCS-C:"
                             & Code_Sets.Code_Set_Id'Image (CS.Char_Data)));
            pragma Debug (O ("TCS-W:"
                             & Code_Sets.Code_Set_Id'Image (CS.Wchar_Data)));

            Start_Encapsulation (Temp_Buf);

            Marshall (Temp_Buf, Unsigned_Long (CS.Char_Data));
            Marshall (Temp_Buf, Unsigned_Long (CS.Wchar_Data));

            Marshall (Buffer, Encapsulate (Temp_Buf));
            Release_Contents (Temp_Buf.all);
            Release (Temp_Buf);
         end;
      end if;

      while not Last (It) loop
         declare
            Temp_Buf : Buffer_Access := new Buffer_Type;

         begin
            case Value (It).all.Kind is
               when Static_Priority =>
                  --  Marshalling a RTCorbaPriority service context

                  Marshall (Buffer, RTCorbaPriority);

                  pragma Debug
                    (O ("Processing RTCorbaPriority service context"));

                  pragma Debug
                    (O ("Priority:"
                        & PolyORB.Tasking.Priorities.External_Priority'Image
                        (Value (It).all.EP)));

                  Start_Encapsulation (Temp_Buf);
                  if Value (It).all.Kind = Static_Priority then
                     Marshall (Temp_Buf,
                               PolyORB.Types.Short (Value (It).all.EP));
                  end if;

                  Marshall (Buffer, Encapsulate (Temp_Buf));
                  Release_Contents (Temp_Buf.all);
                  Release (Temp_Buf);

               when others =>
                  null;
            end case;
         end;

         Next (It);
      end loop;

      pragma Debug (O ("Marshall_Service_Context_List: leave"));
   end Marshall_Service_Context_List;

   -------------------------------------
   -- Unmarshall_Service_Context_List --
   -------------------------------------

   procedure Unmarshall_Service_Context_List
     (Buffer : access Buffers.Buffer_Type;
      QoS    :    out PolyORB.Request_QoS.QoS_Parameter_Lists.List;
      CS     :    out Code_Set_Context_Access)
   is
      use PolyORB.Request_QoS;
      use PolyORB.Request_QoS.QoS_Parameter_Lists;

      Length : constant PolyORB.Types.Unsigned_Long := Unmarshall (Buffer);

   begin
      pragma Debug (O ("Unmarshall_Service_Context_List: enter, length ="
                       & PolyORB.Types.Unsigned_Long'Image (Length)));

      for J in 1 .. Length loop
         declare
            Context_Id   : constant Types.Unsigned_Long := Unmarshall (Buffer);

            Context_Data : aliased Encapsulation := Unmarshall (Buffer);
            Context_Buffer : aliased Buffer_Type;

         begin
            pragma Debug
              (O ("Got context id #"
                  & PolyORB.Types.Unsigned_Long'Image (Context_Id)));

            case Context_Id is
               when CodeSets =>

                  --  Unmarshalling a CodeSets service context

                  Decapsulate (Context_Data'Access, Context_Buffer'Access);

                  CS := new Code_Set_Context;

                  CS.Char_Data :=
                    Code_Sets.Code_Set_Id
                     (Unsigned_Long'(Unmarshall (Context_Buffer'Access)));
                  CS.Wchar_Data :=
                    Code_Sets.Code_Set_Id
                     (Unsigned_Long'(Unmarshall (Context_Buffer'Access)));

                  pragma Debug (O ("Processing CodeSets service context"));

                  pragma Debug (O ("TCS-C:"
                    & Code_Sets.Code_Set_Id'Image (CS.Char_Data)));
                  pragma Debug (O ("TCS-W:"
                    & Code_Sets.Code_Set_Id'Image (CS.Wchar_Data)));

               when RTCorbaPriority =>

                  --  Unmarshalling a RTCorbaPriority service context

                  declare
                     package PTP renames PolyORB.Tasking.Priorities;

                     EP : PolyORB.Types.Short;
                     OP : PTP.ORB_Priority;
                     --  XXX these variables need documentation
                     Ok : Boolean;

                  begin
                     Decapsulate (Context_Data'Access, Context_Buffer'Access);

                     EP := Unmarshall (Context_Buffer'Access);

                     PolyORB.Tasking.Priorities.To_ORB_Priority
                       (PTP.External_Priority (EP), OP, Ok);

                     pragma Debug
                       (O ("Processing RTCorbaPriority service context"));
                     pragma Debug
                       (O ("Priority:" & Types.Short'Image (EP)));

                     Append (QoS, new QoS_Parameter'(
                       Kind => Static_Priority,
                       OP   => OP,
                       EP   => PTP.External_Priority (EP)));
                  end;

               when others =>
                  null;

            end case;
         end;
      end loop;

      pragma Debug (O ("Unmarshall_Service_Context_List: leave"));
   end Unmarshall_Service_Context_List;

end PolyORB.GIOP_P.Service_Contexts;
