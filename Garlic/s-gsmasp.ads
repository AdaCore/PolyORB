--
--  $Id$
--

private package System.Garlic.Serial.Machine_Specific is

   --  This package contains definition for machine specific access
   --  to a serial device. The body of this package must provide a way
   --  of sending and receiving reliably data over a serial link.

   type Serial_Device is private;

   function Open (Description : String) return Serial_Device;
   --  Open a device and return an handler on it. If it's impossible,
   --  then System.RPC.Communication_Error must be raised.

   procedure Close (Device : in Serial_Device);
   --  Close a serial device.

   procedure Send
     (Device : in Serial_Device;
      Data   : access Ada.Streams.Stream_Element_Array);
   --  Send data over a serial link. System.RPC.Communication_Error must be
   --  raised if it is impossible to send data.

   procedure Receive
     (Device : in Serial_Device;
      Data   : access Ada.Streams.Stream_Element_Array);
   --  Receive data from a serial link. Exactly Data'Length bytes will
   --  be received. If it is impossible to receive data, then
   --  System.RPC.Communication_Error must be raised.

private

   type Serial_Device is new Natural;

end System.Garlic.Serial.Machine_Specific;
