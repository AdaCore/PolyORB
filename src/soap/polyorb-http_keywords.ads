package PolyORB.HTTP_Keywords is

   pragma Preelaborate;

   type Methods is (
      --  <ENUM>
      OPTIONS,  --  >> "OPTIONS"
      GET,      --  >> "GET"
      HEAD,     --  >> "HEAD"
      POST,     --  >> "HOST"
      PUT,      --  >> "PUT"
      DELETE,   --  >> "DELETE"
      TRACE,    --  >> "TRACE"
      CONNECT   --  >> "CONNECT"
      --  </ENUM>
   );
   pragma Convention (C, Methods);

   function To_String (Id : Methods) return String;
   function In_Word_Set (S : String) return Methods;

end PolyORB.HTTP_Keywords;
