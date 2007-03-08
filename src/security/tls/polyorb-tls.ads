------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          P O L Y O R B . T L S                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
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

--  A binding for the OpenSSL library

with Ada.Streams;

with PolyORB.Sockets;
with PolyORB.X509;

package PolyORB.TLS is

   type TLS_Method_Type is
     (SSL_2, SSL_2_Client, SSL_2_Server,
      SSL_3, SSL_3_Client, SSL_3_Server,
      TLS_1, TLS_1_Client, TLS_1_Server,
      Any,   Any_Client,   Any_Server);

   type TLS_Verification_Mode_Flag is
     (Peer, Fail_If_No_Peer_Certificate, Client_Once);

   type TLS_Verification_Mode is array (TLS_Verification_Mode_Flag) of Boolean;

   type TLS_Context_Type is private;

   type TLS_Cipher_Type is private;
   No_TLS_Cipher : constant TLS_Cipher_Type;

   type TLS_Cipher_List is array (Positive range <>) of TLS_Cipher_Type;

   type TLS_Socket_Type is private;
   No_TLS_Socket : constant TLS_Socket_Type;

   TLS_Error : exception;

   ----------------------------
   -- TLS context operations --
   ----------------------------

   function Create (Method : TLS_Method_Type) return TLS_Context_Type;
   --  Create a new context with specified Method. Raise TLS_Error on
   --  any error.

   procedure Destroy (Context : in out TLS_Context_Type);
   --  Destroy context

   procedure Use_Certificate
     (Context   : TLS_Context_Type;
      File_Name : String);
   --  Loads certificate from file into context. Raise TLS_Error on any error.

   procedure Use_Certificate
     (Context     : TLS_Context_Type;
      Certificate : PolyORB.X509.Certificate);
   --  Loads certificate into context. Raise TLS_Error on any error.

   procedure Use_Certificate_Chain
     (Context   : TLS_Context_Type;
      File_Name : String);
   --  Loads certificate chain from file into context. Raise TLS_Error on any
   --  error.

   procedure Use_Private_Key
     (Context   : TLS_Context_Type;
      File_Name : String);
   --  Add private key found in file to context. Raise TLS_Error on any error.

   procedure Check_Private_Key (Context : TLS_Context_Type);
   --  Check the consistency of a private key with the corresponding
   --  certificate loaded into context. Raise TLS_Error on any error.

   procedure Load_Verify_Locations
     (Context                    : TLS_Context_Type;
      Certificate_Authority_File : String;
      Certificate_Authority_Path : String);
   --  Specify the location for context, at which Certificate Authority
   --  certificates for verification purposes are located. Raise TLS_Error
   --  on any error.

   procedure Set_Verify_Mode
     (Context : TLS_Context_Type;
      Mode    : TLS_Verification_Mode);
   --  Sets the verification flags for context

   procedure Set_Cipher_List
     (Context : TLS_Context_Type;
      Ciphers : String);
   --  Sets the list of available ciphers for context. Raise TLS_Error on
   --  complete failure (no available ciphers at all).

   function Ciphers_Of (Context  : TLS_Context_Type) return TLS_Cipher_List;
   --  Returns list of available ciphers

   ---------------------------
   -- TLS cipher operations --
   ---------------------------

   function Description_Of (Cipher : TLS_Cipher_Type) return String;
   --  Returns a textual description of the cipher

   ---------------------------
   -- TLS socket operations --
   ---------------------------

   function Create (Context : TLS_Context_Type) return TLS_Socket_Type;
   --  Create new structure for SSL/TLS connection

   procedure Destroy (Socket : in out TLS_Socket_Type);
   --  Destroy connection

   function Cipher_Of
     (Socket   : TLS_Socket_Type;
      Priority : Natural) return String;
   --  Returns the name of cipher listed for Socket with Priority. Returns
   --  an empty string if no cipher with Priority available.

   function Ciphers_Of (Socket  : TLS_Socket_Type) return TLS_Cipher_List;
   --  Returns list of available ciphers

   function Socket_Of (Socket : TLS_Socket_Type) return Sockets.Socket_Type;
   --  Return the underlying socket for the given SSL connection

   procedure Set_Socket (Socket : TLS_Socket_Type; Sock : Sockets.Socket_Type);
   --  Set Sock as input/output facility. Raise TLS_Error on any error.

   procedure Connect_Socket (Socket : TLS_Socket_Type);
   --  Initiates the TLS/SSL handshake with a server. Raise TLS_Error on
   --  any error.

   procedure Accept_Socket (Socket : TLS_Socket_Type);
   --  Waits for a TLS/SSL client to initiate the TLS/SSL handshake.
   --  Raise TLS_Error on any error.

   procedure Close_Socket (Socket : in out TLS_Socket_Type);
   --

   function Peer_Certificate_Of
     (Socket : TLS_Socket_Type) return PolyORB.X509.Certificate;
   --  Returns a X.509 certificate the peer present. Returned ceritificate
   --  should be freed by caller.

   function Pending_Length (Socket : TLS_Socket_Type) return Natural;
   --  Return number of readable bytes buffered in Socket

   procedure Receive_Vector
     (Socket : TLS_Socket_Type;
      Vector : Sockets.Vector_Type;
      Count  : out Ada.Streams.Stream_Element_Count);
   --  Receive data from a socket and scatter it into the set of vector
   --  elements Vector. Count is set to the count of received stream elements.
   --  Raise TLS_Error on SSL socket error.

   procedure Send_Vector
     (Socket : TLS_Socket_Type;
      Vector : Sockets.Vector_Type;
      Count  : out Ada.Streams.Stream_Element_Count);
   --  Transmit data gathered from the set of vector elements Vector to a
   --  socket. Count is set to the count of transmitted stream elements.
   --  Raise TLS_Error on SSL socket error.

private

   --  TLS Context

   type TLS_Context_Record is null record;
   pragma Convention (C, TLS_Context_Record);

   type TLS_Context_Type is access all TLS_Context_Record;

   --  TLS Cipher

   type TLS_Cipher_Record is null record;
   pragma Convention (C, TLS_Cipher_Record);

   type TLS_Cipher_Type is access all TLS_Cipher_Record;

   No_TLS_Cipher : constant TLS_Cipher_Type := null;

   --  TLS Socket

   type TLS_Socket_Record is null record;
   pragma Convention (C, TLS_Socket_Record);

   type TLS_Socket_Type is access all TLS_Socket_Record;

   No_TLS_Socket : constant TLS_Socket_Type := null;

end PolyORB.TLS;
