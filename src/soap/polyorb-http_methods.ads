package PolyORB.HTTP_Methods is

   type Method is
     (
      --  <ENUM>
      OPTIONS,  --  >> "OPTIONS"
      GET,      --  >> "GET"
      HEAD,     --  >> "HEAD"
      POST,     --  >> "POST"
      PUT,      --  >> "PUT"
      DELETE,   --  >> "DELETE"
      TRACE,    --  >> "TRACE"
      CONNECT   --  >> "CONNECT"
      --  </ENUM>
      );
   pragma Convention (C, Method);

   function To_String (Id : Method) return String;
   function In_Word_Set (S : String) return Method;

end PolyORB.HTTP_Methods;
