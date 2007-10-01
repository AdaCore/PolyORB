------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          P O L Y O R B . S S L                           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2005 Free Software Foundation, Inc.             --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  A binding for the OpenSSL library

with Ada.Streams;

with PolyORB.Sockets;

package PolyORB.SSL is

   type SSL_Context_Type is private;

   type SSL_Socket_Type is private;
   No_SSL_Socket : constant SSL_Socket_Type;

   type SSL_Method_Type is
     (SSL_2, SSL_2_Client, SSL_2_Server,
      SSL_3, SSL_3_Client, SSL_3_Server,
      TLS_1, TLS_1_Client, TLS_1_Server,
      Any,   Any_Client,   Any_Server);

   type SSL_Verification_Mode_Flag is
     (Peer, Fail_If_No_Peer_Certificate, Client_Once);

   type SSL_Verification_Mode is array (SSL_Verification_Mode_Flag) of Boolean;

   type SSL_Cipher_Type is private;
   No_SSL_Cipher : constant SSL_Cipher_Type;

   type SSL_Cipher_Array is array (Positive range <>) of SSL_Cipher_Type;

   SSL_Error : exception;

   ------------------------
   -- Context operations --
   ------------------------

   procedure Create_Context
     (Context           : out SSL_Context_Type;
      Method            : SSL_Method_Type;
      Private_Key_File  : String;
      Certificate_File  : String;
      CA_File           : String := "";
      CA_Path           : String := "";
      Verification_Mode : SSL_Verification_Mode := (others => False));
   --  Create a new SSL context with the specified Method; load private key
   --  and certificate from files. CA_File and CA_Path define file and path
   --  of trusted CA sertificates repository. Raises SSL_Error on any
   --  error.

   procedure Load_Client_CA
     (Context : SSL_Context_Type;
      CA_File : String);
   --  Set the list of CAs sent to the client when requesting a client
   --  certificate from file. Relevant only to servers. Raises SSL_Error on
   --  any error.

   procedure Destroy_Context (Context : SSL_Context_Type);
   --  Destroy SSL context

   function Ciphers_Of (Context : SSL_Context_Type) return SSL_Cipher_Array;
   --  Return list of available ciphers

   function Verification_Mode_Of
     (Context : SSL_Context_Type) return SSL_Verification_Mode;
   --  Return current verification mode value

   ---------------------------
   -- SSL socket operations --
   ---------------------------

   procedure Accept_Socket
     (Server  : Sockets.Socket_Type;
      Context : SSL_Context_Type;
      Socket  : out SSL_Socket_Type;
      Address : out Sockets.Sock_Addr_Type);
   --  Extract the first pending incoming connection from the queue.
   --  Create a new connected socket with the same properties as Server,
   --  allocates a new SSL context, and negotiate an SSL connection.
   --  On return, Address is the address of the remote endpoint.
   --  Raises Socket_Error on socket error and SSL_Error on SSL connection
   --  negotiation error.

   procedure Connect_Socket
     (Sock    : Sockets.Socket_Type;
      Context : SSL_Context_Type;
      Socket  : out SSL_Socket_Type;
      Address : in out Sockets.Sock_Addr_Type);
   --  Make a connection to a remote SSL access point with the given
   --  Address, using SSL parameters specified by Context.
   --  Raises Socket_Error on socket error and SSL_Error on SSL connection
   --  negotiation error.

   procedure Close_Socket (Socket : SSL_Socket_Type);
   --  Close a socket

   function Socket_Of (Socket : SSL_Socket_Type) return Sockets.Socket_Type;
   --  Return the underlying socket for the given SSL connection

   function Pending_Length (Socket : SSL_Socket_Type) return Natural;
   --  Return number of readable bytes buffered in Socket

   procedure Receive_Vector
     (Socket : SSL_Socket_Type;
      Vector : Sockets.Vector_Type;
      Count  : out Ada.Streams.Stream_Element_Count);
   --  Receive data from a socket and scatter it into the set of vector
   --  elements Vector. Count is set to the count of received stream elements.
   --  Raise SSL_Error on SSL socket error.

   procedure Send_Vector
     (Socket : SSL_Socket_Type;
      Vector : Sockets.Vector_Type;
      Count  : out Ada.Streams.Stream_Element_Count);
   --  Transmit data gathered from the set of vector elements Vector to a
   --  socket. Count is set to the count of transmitted stream elements.
   --  Raise SSL_Error on SSL socket error.

   function Ciphers_Of (Socket : SSL_Socket_Type) return SSL_Cipher_Array;
   --  Return list of available ciphers

   ------------------------------
   -- Miscellaneous operations --
   ------------------------------

   function Get_Errors_String return String;
   --  Return description string for all errors from error queue

   function Description_Of (Cipher : SSL_Cipher_Type) return String;
   --  XXX comment required???

private

   pragma Linker_Options ("-lpolyorb-ssl");

   type Context_Record is null record;
   pragma Convention (C, Context_Record);

   type SSL_Context_Type is access all Context_Record;

   type Socket_Record is null record;
   pragma Convention (C, Socket_Record);

   type SSL_Socket_Type is access all Socket_Record;

   type SSL_Cipher_Record is null record;
   pragma Convention (C, SSL_Cipher_Record);

   type SSL_Cipher_Type is access all SSL_Cipher_Record;

   No_SSL_Cipher : constant SSL_Cipher_Type := null;
   No_SSL_Socket : constant SSL_Socket_Type := null;

end PolyORB.SSL;
