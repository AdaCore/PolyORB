--  A filter that slices a stream into a set of known-length
--  messages.

--  $Id$

with Droopi.Filters.Interface; use Droopi.Filters.Interface;

package body Droopi.Filters.Slicers is

   use Ada.Streams;
   use Droopi.Buffers;

   procedure Create
     (Fact   : access Slicer_Factory;
      Slicer : out Filter_Access)
   is
      Res : constant Filter_Access := new Slicer_Filter;
   begin
      Slicer_Filter (Res.all).Data_Expected := 0;
      Slicer := Res;
   end Create;

   function Handle_Message
     (F : access Slicer_Filter;
      S : Components.Message'Class)
     return Components.Message'Class
   is
      Res : Components.Null_Message;
   begin
      if S in Data_Expected then
         declare
            DEM : Data_Expected renames Data_Expected (S);
         begin

            pragma Assert (F.Data_Expected = 0
                           and then F.In_Buf = null);
            F.Data_Expected := DEM.Max;
            F.In_Buf := DEM.In_Buf;
            F.Buffer_Position := CDR_Position (F.In_Buf);
         end;
      elsif S in Data_Indication then
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
               return Emit
                 (F.Upper,
                  Data_Indication'(Root_Data_Unit with null record));
            end if;
         end;

      elsif S in Disconnect_Indication then
         return Emit
           (F.Upper,
            Disconnect_Indication'(Root_Data_Unit with null record));

      elsif S in Data_Out then
         return Emit (F.Lower, S);
      end if;

      return Res;
   end Handle_Message;

end Droopi.Filters.Slicers;
