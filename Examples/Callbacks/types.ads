package Types is

   pragma Pure;

   type Query is new Integer;
   No_Query : constant Query := Query'First;

   type Reply is new Integer;
   No_Reply : constant Reply := Reply'First;

end Types;
