--  A filter that slices a stream into a set of known-length
--  messages.

--  $Id$

with PolyORB.Filters.Interface;
with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);

package body PolyORB.Filters.Slicers is

   use Ada.Streams;

   use PolyORB.Buffers;
   use PolyORB.Filters.Interface;
   use PolyORB.Log;

   package L is new PolyORB.Log.Facility_Log ("polyorb.filters.slicers");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;


   procedure Create
     (Fact   : access Slicer_Factory;
      Slicer : out Filter_Access)
   is
      use PolyORB.Components;

      Res : constant Filter_Access := new Slicer_Filter;
   begin
      Set_Allocation_Class (Res.all, Dynamic);
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
            pragma Debug (O ("Expecting" & DEM.Max'Img
                             & " bytes."));

            pragma Assert (True
              and then F.Data_Expected = 0
              and then F.In_Buf = null
              and then DEM.Max /= 0
              and then DEM.In_Buf /= null);

            F.Data_Expected := DEM.Max;
            F.In_Buf := DEM.In_Buf;
            F.Buffer_Length := Length (F.In_Buf);

            return Emit
              (F.Lower,
               Data_Expected'
               (Max => F.Data_Expected, In_Buf => F.In_Buf));
         end;

      elsif S in Data_Indication then
         declare
            Data_Received : constant Stream_Element_Count
              := Length (F.In_Buf) - F.Buffer_Length;
         begin
            pragma Debug (O ("Expected" & F.Data_Expected'Img
                             & " bytes, received"
                             & Data_Received'Img));

            if F.In_Buf = null
              or else Data_Received > F.Data_Expected
            then
               raise Unexpected_Data;
               --  This exception will be propagated to the ORB.
            end if;

            F.Data_Expected := F.Data_Expected - Data_Received;
            F.Buffer_Length := Length (F.In_Buf);

            if F.Data_Expected = 0 then
               F.In_Buf := null;
               return Emit
                 (F.Upper,
                  Data_Indication'(Root_Data_Unit with null record));
            else
               pragma Debug (O ("Expecting" & F.Data_Expected'Img
                                & " further bytes."));
               Emit_No_Reply
                 (F.Lower,
                  Data_Expected'
                  (Max => F.Data_Expected, In_Buf => F.In_Buf));
            end if;
         end;

      elsif False
        or else S in Connect_Indication
        or else S in Connect_Confirmation
        or else S in Disconnect_Indication
        or else S in Set_Server
      then
         return Emit (F.Upper, S);
      elsif S in Data_Out then
         return Emit (F.Lower, S);
      else
         raise Unhandled_Message;
      end if;

      return Res;
   end Handle_Message;

end PolyORB.Filters.Slicers;
