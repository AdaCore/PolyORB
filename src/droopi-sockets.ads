with Ada.Streams;

package Droopi.Sockets is

   pragma Preelaborate;

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


   type Socket_Set_Type is private;

   procedure Clear (Set : in out Socket_Set_Type; Socket : in Socket_Type);
   procedure Set   (Set : in out Socket_Set_Type; Socket : in Socket_Type);
   procedure Zero  (Set : in out Socket_Set_Type);

   function Empty
     (Set : Socket_Set_Type) return Boolean;

   function Is_Set
     (Set    : Socket_Set_Type;
      Socket : Socket_Type) return Boolean;

   type Microseconds is new Natural;
   Forever : constant Microseconds := Microseconds'Last;


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

   procedure Select_Socket
     (R_Socket_Set : in out Socket_Set_Type;
      W_Socket_Set : in out Socket_Set_Type;
      Timeout      : in Microseconds := Forever);

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

   type Socket_Set_Record;
   type Socket_Set_Type is access all Socket_Set_Record;

end Droopi.Sockets;
