------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   P O L Y O R B . Q O S . E X C E P T I O N _ I N F O R M A T I O N S    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2007, Free Software Foundation, Inc.          --
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

with Ada.Strings.Fixed;

with PolyORB.Buffers;
with PolyORB.Initialization;
with PolyORB.QoS.Service_Contexts;
with PolyORB.Representations.CDR.Common;
with PolyORB.Request_QoS;
with PolyORB.Utils.Strings;

package body PolyORB.QoS.Exception_Informations is

   use PolyORB.Buffers;
   use PolyORB.QoS.Service_Contexts;
   use PolyORB.Representations.CDR.Common;

   function To_AdaExceptionInformation_Service_Context
     (QoS : QoS_Parameter_Access)
     return Service_Context;

   function To_Ada_Exception_Information_Parameter
     (SC : Service_Context)
     return QoS_Parameter_Access;

   procedure Initialize;

   -------------------------------
   -- Get_Exception_Information --
   -------------------------------

   function Get_Exception_Information
     (R : Requests.Request_Access) return String
   is
      QoS : constant QoS_Ada_Exception_Information_Parameter_Access :=
              QoS_Ada_Exception_Information_Parameter_Access
                 (PolyORB.Request_QoS.Extract_Reply_Parameter
                   (PolyORB.QoS.Ada_Exception_Information, R));
   begin
      if QoS /= null then
         return Types.To_Standard_String (QoS.Exception_Information);
      else
         return "";
      end if;
   end Get_Exception_Information;

   ---------------------------
   -- Get_Exception_Message --
   ---------------------------

   function Get_Exception_Message
     (R : Requests.Request_Access) return String
   is
      Exception_Information    : constant String :=
                                   Get_Exception_Information (R);

      Exception_Message_Marker : constant String :=
                                   ASCII.LF & "Message: ";
      First, Last : Integer;
   begin
      --  The expected format of the exception information is:

      --  "Exception name: " & Excception_Name & ASCII.LF &
      --  "Message: " & Exception_Message & ASCII.LF

      First := Ada.Strings.Fixed.Index
                 (Source  => Exception_Information,
                  Pattern => Exception_Message_Marker);

      --  If separator is not found, just return entire Exception_Information

      if First = 0 then
         First := Exception_Information'First;
      else
         First := First + Exception_Message_Marker'Length;
      end if;

      --  Strip trailing newline

      Last := Exception_Information'Last;
      if Last >= First and then Exception_Information (Last) = ASCII.LF then
         Last := Last - 1;
      end if;

      --  Return appropriate slice

      return Exception_Information (First .. Last);
   end Get_Exception_Message;

   -------------------------------
   -- Set_Exception_Information --
   -------------------------------

   procedure Set_Exception_Information
     (Request    : PolyORB.Requests.Request_Access;
      Occurrence : Ada.Exceptions.Exception_Occurrence)
   is
      use PolyORB.QoS.Exception_Informations;
   begin
      Request_QoS.Add_Reply_QoS
        (Request,
         PolyORB.QoS.Ada_Exception_Information,
           new QoS_Ada_Exception_Information_Parameter'
             (Kind                  =>
                QoS.Ada_Exception_Information,
              Exception_Information =>
                Types.To_PolyORB_String
                  (Ada.Exceptions.Exception_Information (Occurrence))));
   end Set_Exception_Information;

   ------------------------------------------------
   -- To_AdaExceptionInformation_Service_Context --
   ------------------------------------------------

   function To_AdaExceptionInformation_Service_Context
     (QoS : QoS_Parameter_Access)
     return Service_Context
   is
      Result : Service_Context := (AdaExceptionInformation, null);

   begin
      if QoS = null then
         return Result;
      end if;

      declare
         AEI    : QoS_Ada_Exception_Information_Parameter
           renames QoS_Ada_Exception_Information_Parameter (QoS.all);
         Buffer : Buffer_Access := new Buffer_Type;

      begin
         Start_Encapsulation (Buffer);

         Marshall_Latin_1_String (Buffer, AEI.Exception_Information);
         Result.Context_Data := new Encapsulation'(Encapsulate (Buffer));

         Release (Buffer);
      end;

      return Result;
   end To_AdaExceptionInformation_Service_Context;

   --------------------------------------------
   -- To_Ada_Exception_Information_Parameter --
   --------------------------------------------

   function To_Ada_Exception_Information_Parameter
     (SC : Service_Context)
     return QoS_Parameter_Access
   is
      Buffer                : aliased Buffer_Type;
      Exception_Information : PolyORB.Types.String;

   begin
      Decapsulate (SC.Context_Data, Buffer'Access);

      Exception_Information := Unmarshall_Latin_1_String (Buffer'Access);

      return
        new QoS_Ada_Exception_Information_Parameter'
        (Kind                  => Ada_Exception_Information,
         Exception_Information => Exception_Information);
   end To_Ada_Exception_Information_Parameter;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Register
        (Ada_Exception_Information,
         To_AdaExceptionInformation_Service_Context'Access);
      Register
        (AdaExceptionInformation,
         To_Ada_Exception_Information_Parameter'Access);
   end Initialize;

begin
   declare
      use PolyORB.Initialization;
      use PolyORB.Initialization.String_Lists;
      use PolyORB.Utils.Strings;
   begin
      Register_Module
        (Module_Info'
         (Name      => +"qos.exception_information",
          Conflicts => Empty,
          Depends   => Empty,
          Provides  => Empty,
          Implicit  => False,
          Init      => Initialize'Access,
          Shutdown  => null));
   end;
end PolyORB.QoS.Exception_Informations;
