----------------------------------------------------------------------------
----                                                                    ----
----       This example was written for the first public demo           ----
----                      of AdaBroker, 04/02/1999                      ----
----                                                                    ----
----------------------------------------------------------------------------


with Ada.Command_Line ; use Ada.Command_Line ;
with Text_Io ;
with Corba, Corba.Orb, Corba.Boa, Corba.Object ;
with Mpeg ; use Mpeg ;
with Mpeg.Mpegdecoder ; use Mpeg.Mpegdecoder ;
with Ada.Sequential_Io ;

procedure Client is

   Orb : Corba.Orb.Object := Corba.Orb.Orb_Init("omniORB2");
   Boa : Corba.Boa.Object := Corba.Orb.Boa_Init(Orb, "omniORB2_BOA") ;

   Ior : CORBA.String ;
   Mp : Mpeg.Mpegdecoder.Ref ;



   package Mpeg_Frame_Io is new Ada.Sequential_Io(Mpegframe) ;
   use Mpeg_Frame_Io ;
   Mpeg_File : File_Type ;
   Frame : Mpegframe ;

begin

   if Argument_Count < 2 then
      Text_Io.Put_Line ("usage : client <IOR_string_from_server> <mpeg-file-name>") ;
      return ;
   end if ;

   -- Get the Ref
   IOR := Corba.To_Corba_String(Argument(1)) ;
   Corba.Orb.String_To_Object(Ior, Mp) ;

   -- checking if it worked
   if Is_Nil(Mp) then
      Text_Io.Put_Line("main : cannot invoke on a nil reference") ;
      return ;
   end if ;


   Open(Mpeg_File, In_File, Argument(2)) ;

   while not End_Of_File(Mpeg_File) loop
      Read(Mpeg_File, Frame) ;
      Putframe(Mp, Frame) ;
   end loop ;

   Close(Mpeg_File) ;

   Text_Io.Put_Line("Fin du film") ;

end Client ;




