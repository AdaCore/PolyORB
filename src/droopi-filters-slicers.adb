--  A filter that slices a stream into a set of known-length
--  messages.

--  $Id$

package body Droopi.Filters.Slicers is

   pragma Elaborate_Body;

   procedure Create
     (Fact   : access Slicer_Factory;
      Slicer : out Filter_Access)
   is
      Res : constant Filter_Access := new Slicer_Filter;
   begin
      Slicer_Filter (Res.all).Data_Expected := 0;
      Upper := Res;
   end Create;

   procedure Handle_Message
     (F : access Slicer_Filter;
      S : Data_Unit) is
   begin
      case S.Kind is

         when Data_Expected =>
            pragma Assert (F.Data_Expected = 0
                           and then F.In_Buf = null);
            F.Data_Expected := S.Max;
            F.In_Buf := S.In_Buf;
            F.Buffer_Position := CDR_Position (F.In_Buf);

         when Data_Indication =>
            declare
               Data_Received : constant Stream_Element_Count
                 := CDR_Position (F.In_Buf) - F.Buffer_Position;
            begin
               if F.In_Buf = null
                 or else Data_Received > F.Data_Expected
               then
                  raise Unexpected_Data;
                  --  This exception will be propagated to the ORB.
               end if;

               F.Data_Expected := F.Data_Expected - Data_Received;
               F.Buffer_Position := CDR_Position (F.In_Buf);

               if F.Data_Expected = 0 then
                  F.In_Buf := null;
                  Handle_Message (F.Upper, Data_Unit'(Kind => Data_Indication));
               end if;
            end;

         when Disconnect_Indication =>
            Handle_Message (F.Upper, Data_Unit'(Kind => Disconnect_Indication));

         when Data_Out =>
         Handle_Message (F.Lower, S);
      end case;
   end Handle_Message;

end Droopi.Filters.Slicers;
