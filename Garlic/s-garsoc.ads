------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                S Y S T E M . G A R L I C . S O C K E T S                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2001 Free Software Foundation, Inc.           --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
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
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Streams;

package System.Garlic.Sockets is

   ------------------------
   -- Miscellaneous Type --
   ------------------------

   type Mode_Type is (SOCK_STREAM, SOCK_DGRAM);

   type Option_Type is (SO_REUSEADDR, SO_KEEPALIVE);

   type Port_Type is new Natural;
   Null_Port : constant Port_Type := 0;


   -----------------
   -- Socket Type --
   -----------------

   type Socket_Type is private;
   Null_Socket : constant Socket_Type;

   Socket_Error : exception;

   function Image (Socket : Socket_Type) return String;


   -------------------------
   -- Socket Address Type --
   -------------------------

   --  Include either an inet address v4, an inet address v6 or an
   --  inet address for Java.

   type Sock_Addr_Type is abstract tagged
      record
         Port : Port_Type;
      end record;

   type Sock_Addr_Access is access all Sock_Addr_Type'Class;

   function Image (Value : Sock_Addr_Type) return String is abstract;
   --  The dotted form corresponding to an IP address

   procedure Bind_Socket
     (Socket : in Socket_Type;
      MyAddr : in Sock_Addr_Type) is abstract;
   --  int bind(int sockfd, struct sockaddr * my_addr, socklen_t addrlen);
   --  Raise Socket_Error on error.

   procedure Connect_Socket
     (Socket : in Socket_Type;
      Server : in Sock_Addr_Type) is abstract;
   --  int connect (int sockfd, const struct sockaddr * serv_addr,
   --  socklen_t addrlen);
   --  Raise Socket_Error on error.

   function Value
     (Image : String)
     return Sock_Addr_Type'Class;
   --  Address of an IP name or a dotted form

   function Host_Name return String;
   --  Return the name of the current host

   function Official_Name (Image : String) return String;
   --  Return the official name of an IP name or a dotted form. The name
   --  will be returned in lower case as the DNS is case insensitive.

   Naming_Error : exception;
   --  This exception is raised when a name cannot be resolved


   function New_Socket
     (Mode : Mode_Type := SOCK_STREAM)
     return Socket_Type;
   --  int socket(int domain, int type, int protocol);
   --  domain is automatically set to AF_INET
   --  type is SOCK_STREAM (default) or SOCK_DGRAM
   --  Return Null_Socket on error.

   function Accept_Socket
     (Socket  : Socket_Type)
     return Socket_Type;
   --  int accept(int s, struct sockaddr * addr, socklen_t * addrlen);
   --  addr and addrlen are automatically set and returned values not used
   --  Raise Socket_Error on error.

   procedure Close_Socket
     (Socket : in Socket_Type);
   --  int close(int fd);
   --  Fail silently.

   function Get_Socket_Name
     (Socket : in Socket_Type)
     return Sock_Addr_Type'Class;
   --  int getsockname (int s, struct sockaddr * name, socklen_t * namelen);
   --  Raise Socket_Error on error.

   procedure Listen_Socket
     (Socket : in Socket_Type;
      Length : in Positive := 15);
   --  int listen(int s, int backlog);
   --  Raise Socket_Error on error.

   procedure Receive_Socket
     (Socket : in Socket_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   procedure Send_Socket
     (Socket : in  Socket_Type;
      Item   : in  Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   procedure Set_Socket_Option
     (Socket : in Socket_Type;
      Option : in Option_Type);

private

   type Socket_Type is new Integer;
   Null_Socket : constant Socket_Type := -1;

end System.Garlic.Sockets;
