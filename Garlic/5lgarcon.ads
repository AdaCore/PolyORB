--  This package has been generated automatically on:
--  Linux arlequin 2.2.17 #2 Wed Oct 18 18:43:33 CEST 2000 i686 unknown
--  Generation date: Thu Dec 14 18:39:17 CET 2000
--  Any change you make here is likely to be lost !
package System.Garlic.Constants is
   Tcp_Nodelay          : constant := 16#0001#;
   Af_Inet              : constant := 16#0002#;
   Pf_Inet              : constant := 16#0002#;
   Sock_Stream          : constant := 16#0001#;
   Sock_Dgram           : constant := 16#0002#;
   Eintr                : constant := 16#0004#;
   Eagain               : constant := 16#000B#;
   Ewouldblock          : constant := 16#000B#;
   Einprogress          : constant := 16#0073#;
   Ealready             : constant := 16#0072#;
   Eisconn              : constant := 16#006A#;
   Econnrefused         : constant := 16#006F#;
   Fndelay              : constant := 16#0800#;
   Fasync               : constant := 16#2000#;
   F_Getfl              : constant := 16#0003#;
   F_Setfl              : constant := 16#0004#;
   F_Setown             : constant := 16#0008#;
   So_Rcvbuf            : constant := 16#0008#;
   So_Reuseaddr         : constant := 16#0002#;
   So_Keepalive         : constant := 16#0009#;
   Sol_Socket           : constant := 16#0001#;
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
   I_Setsig             : constant := 16#5309#;
   S_Rdnorm             : constant := 16#0040#;
   S_Wrnorm             : constant := 16#0004#;
end System.Garlic.Constants;
