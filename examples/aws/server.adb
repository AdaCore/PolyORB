with Ada.Text_IO; use Ada.Text_IO;

with AWS.Server;
with SOAP.Types;
with SOAP.Parameters;
with SOAP.Message;
with SOAP.Message.Payload;
with SOAP.Message.Response;
with AWS.Server.Servants;
with AWS.Status;
with AWS.Response;
with AWS.Parameters;

--  with PolyORB.Setup.Ravenscar_TP_Server;
--  pragma Elaborate_All (PolyORB.Setup.Ravenscar_TP_Server);
--  pragma Warnings (Off, PolyORB.Setup.Ravenscar_TP_Server);

--  with PolyORB.Setup.No_Tasking_Server;
--  pragma Elaborate_All (PolyORB.Setup.No_Tasking_Server);
--  pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);

with PolyORB.Setup.Thread_Per_Request_Server;
pragma Elaborate_All (PolyORB.Setup.Thread_Per_Request_Server);
pragma Warnings (Off, PolyORB.Setup.Thread_Per_Request_Server);

with PolyORB.Types;
with PolyORB.References.IOR;
with PolyORB.References.URI;
with PolyORB.Calendar;

procedure Server is

   function Callback
     (Request : in AWS.Status.Data)
      return AWS.Response.Data;

   function Callback
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      use AWS.Status;
      use AWS.Response;

   begin
      if Is_SOAP (Request) then
         declare
            use SOAP.Types;
            use SOAP.Parameters;
            use SOAP.Message;
            use SOAP.Message.Payload;

            SOAP_Object : SOAP.Message.Payload.Object
              := AWS.Status.Payload (Request);
            P_List : SOAP.Parameters.List := Parameters (SOAP_Object);
            Mesg : constant String := SOAP.Parameters.Get (P_List, "Mesg");
            R  : SOAP.Message.Response.Object;
            RP : SOAP.Parameters.List;
         begin
            Put_Line ("Server: "
                      & Natural'Image (Argument_Count (P_List))
                      & " SOAP parameters");
            Put_Line ("Server: method is "
                      & Procedure_Name (SOAP_Object));
            Put_Line ("Server: echoing """ & Mesg & """");
            R := SOAP.Message.Response.From (SOAP_Object);
            RP := +S (Mesg, "result");
            SOAP.Message.Set_Parameters (R, RP);
            return AWS.Response.Build (R);
         end;
      else
         declare
            P_List : constant AWS.Parameters.List
              := AWS.Status.Parameters (D => Request);
            Mesg : constant String := AWS.Parameters.Get (P_List, "Mesg");
         begin
            Put_Line ("Server: the request is a web one");
            Put_Line ("Server: echoing """ & Mesg & """");
            return AWS.Response.Build ("text/html", Mesg);
         end;
      end if;

   end Callback;

begin
   Put_Line ("initializing PolyORB");
   AWS.Server.Initialization;
   Put_Line ("initialized");

   declare
      SOAP_Server : AWS.Server.Servants.SOAP_Servant;
      Web_Server : AWS.Server.Servants.Web_Servant;
   begin
      pragma Warnings (Off);
      declare
         use PolyORB.Calendar;
         Horloge1 : Time_Type'Class := Clock;
         Horloge2 : Time_Type_Access := Create;

         Year     : Year_Number;
         Month    : Month_Number;
         Day      : Day_Number;
         Seconds  : Day_Duration;
         Int_Secs : Integer;

      begin
         Horloge2.all := Horloge1 + 0.5;

         if Horloge1 > Horloge2.all then
            Put_Line ("horloge1 > horloge2 ??");
         else
            Put_Line ("horloge2 > horloge1 : ok");
         end if;

         Split (Horloge1,
                Year => Year,
                Month => Month,
                Day => Day,
                Seconds => Seconds);

         Put_Line ("the date is "
                   & Year_Number'Image (Year)
                   & Month_Number'Image (Month)
                   & Day_Number'Image (Day));

         Int_Secs := Integer (Seconds);

         Put_Line ("the time is (" & Day_Duration'Image (Seconds)
                   & "[" & Integer'Image (Int_Secs)
                   & "])"
                   & Integer'Image (Int_Secs / 3600)
                   & Integer'Image ((Int_Secs mod 3600) / 60)
                   & Integer'Image ((Int_Secs mod 60)));

         Destroy (Horloge2);
      end;
         pragma Warnings (On);

      Put_Line ("starting servers");
      AWS.Server.Start (SOAP_Server, "soap_server",
                        Max_Connection => 1,
                        Callback       => Callback'Unrestricted_Access);
      AWS.Server.Start (Web_Server, "web_server",
                        Max_Connection => 1,
                        Callback       => Callback'Unrestricted_Access);
      Put_Line ("servers started");

      Put_Line ("SOAP_Server:");
      Put_Line (PolyORB.Types.To_String
                (PolyORB.References.URI.Object_To_String
                 (AWS.Server.Get_Server_Reference (SOAP_Server))));
      Put_Line ((PolyORB.References.IOR.Object_To_String
                 (AWS.Server.Get_Server_Reference (SOAP_Server))));

      Put_Line ("Web_Server:");
      Put_Line (PolyORB.Types.To_String
                (PolyORB.References.URI.Object_To_String
                 (AWS.Server.Get_Server_Reference (Web_Server))));
      Put_Line ((PolyORB.References.IOR.Object_To_String
                 (AWS.Server.Get_Server_Reference (Web_Server))));


      AWS.Server.Run;
   end;

end Server;



