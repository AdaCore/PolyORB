with PolyORB.Log;
with PolyORB.Representations.CDR;
with PolyORB.Types;

package body PolyORB.GIOP_P.Service_Contexts is

   use PolyORB.Log;
   use PolyORB.Representations.CDR;

   package L is
      new PolyORB.Log.Facility_Log ("polyorb.giop_p.service_contexts");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   -----------------------------------
   -- Marshall_Service_Context_List --
   -----------------------------------

   procedure Marshall_Service_Context_List
     (Buffer : access Buffers.Buffer_Type) is
   begin
      Marshall (Buffer, Types.Unsigned_Long (0));
   end Marshall_Service_Context_List;

   -------------------------------------
   -- Unmarshall_Service_Context_List --
   -------------------------------------

   procedure Unmarshall_Service_Context_List
     (Buffer : access Buffers.Buffer_Type)
   is
      Length : constant PolyORB.Types.Unsigned_Long := Unmarshall (Buffer);

   begin
      pragma Debug (O ("Unmarshall_Service_Context_List: enter, length ="
                       & PolyORB.Types.Unsigned_Long'Image (Length)));

      for J in 1 .. Length loop
         declare
            Context_Id   : constant Types.Unsigned_Long := Unmarshall (Buffer);
            Context_Data : constant Encapsulation := Unmarshall (Buffer);
            pragma Warnings (Off);
            pragma Unreferenced (Context_Id, Context_Data);
            pragma Warnings (On);
         begin
            null;
         end;
      end loop;

      pragma Debug (O ("Unmarshall_Service_Context_List: leave"));
   end Unmarshall_Service_Context_List;

end PolyORB.GIOP_P.Service_Contexts;
