--  This package has been generated automatically on:
--  FreeBSD antinea 5.0-CURRENT FreeBSD 5.0-CURRENT #5: Thu May 4 17:41:50
--  CEST 2000 root@antinea:/usr/objsrc/src/sys/compile/ANTINEA i386
--  Generation date: Mon May 22 16:05:52 CEST 2000
--  Any change you make here is likely to be lost !
package Sockets.Constants is
   Tcp_Nodelay          : constant := 16#0001#;
   Af_Inet              : constant := 16#0002#;
   Af_Unix              : constant := 16#0001#;
   Sock_Stream          : constant := 16#0001#;
   Sock_Dgram           : constant := 16#0002#;
   Eintr                : constant := 16#0004#;
   Eagain               : constant := 16#0023#;
   Ewouldblock          : constant := 16#0023#;
   Einprogress          : constant := 16#0024#;
   Ealready             : constant := 16#0025#;
   Eisconn              : constant := 16#0038#;
   Econnrefused         : constant := 16#003D#;
   Fndelay              : constant := 16#0004#;
   Fasync               : constant := 16#0040#;
   F_Getfl              : constant := 16#0003#;
   F_Setfl              : constant := 16#0004#;
   F_Setown             : constant := 16#0006#;
   So_Rcvbuf            : constant := 16#1002#;
   So_Reuseaddr         : constant := 16#0004#;
   Sol_Socket           : constant := 16#FFFF#;
   Sigterm              : constant := 16#000F#;
   Sigkill              : constant := 16#0009#;
   O_Rdonly             : constant := 16#0000#;
   O_Wronly             : constant := 16#0001#;
   O_Rdwr               : constant := 16#0002#;
   Host_Not_Found       : constant := 16#0001#;
   Try_Again            : constant := 16#0002#;
   No_Recovery          : constant := 16#0003#;
   No_Data              : constant := 16#0004#;
   No_Address           : constant := 16#0004#;
   Pollin               : constant := 16#0001#;
   Pollpri              : constant := 16#0002#;
   Pollout              : constant := 16#0004#;
   Pollerr              : constant := 16#0008#;
   Pollhup              : constant := 16#0010#;
   Pollnval             : constant := 16#0020#;
   I_Setsig             : constant := -1;
   S_Rdnorm             : constant := -1;
   S_Wrnorm             : constant := -1;
   Ipproto_Ip           : constant := 16#0000#;
   Ip_Add_Membership    : constant := 16#000C#;
   Ip_Multicast_Loop    : constant := 16#000B#;
   Ip_Multicast_Ttl     : constant := 16#000A#;
   Ip_Drop_Membership   : constant := 16#000D#;
end Sockets.Constants;
