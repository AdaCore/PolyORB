----------------------------------------------------------------------------
----                                                                    ----
----     This in an example which is hand-written                       ----
----     for the echo object (corresponds to eg2_clt.cc in omniORB      ----
----                                                                    ----
----                client                                              ----
----                                                                    ----
----                authors : Fabien Azavant, Sebastien Ponce           ----
----                                                                    ----
----------------------------------------------------------------------------


with Ada.Command_Line ;
with Text_Io ; use Text_Io ;
with Corba, Corba.Orb, Corba.Boa, Corba.Object ;
with All_Functions ; use All_Functions ;
use Corba ;

procedure Client is

   -- Initialization of the ORB
   Orb : Corba.Orb.Object := Corba.Orb.Orb_Init("omniORB2");

   Sent_Msg, Rcvd_Msg, IOR : CORBA.String ;

   myobj : All_Functions.Ref ;
   I, J, K, L, M : Corba.Short ;

begin

   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line ("usage : client <IOR_string_from_server>") ;
      return ;
   end if ;

   -- transforms the Ada string into Corba.String
   IOR := Corba.To_Corba_String(Ada.Command_Line.Argument(1)) ;

   -- getting the Corba.Object
   Corba.Orb.String_To_Object(IOR, myobj) ;

   -- checking if it worked
   if All_Functions.Is_Nil(myobj) then
      Put_Line("main : cannot invoke on a nil reference") ;
      return ;
   end if ;

   Put_Line("Setting the attribute to 24 ") ;
   Set_The_Attribute(Myobj, Corba.Short(24)) ;
   Put_Line("Setting the attribute -> OK ") ;
   Put_Line("") ;

   Put_Line("Getting the attribute ... ") ;
   I := Get_The_Attribute(Myobj) ;
   Put_Line("Got the attribute -> " & Corba.Short'Image(I)) ;
   Put_Line("") ;

   Put_Line("Getting the readonly attribute : expecting 18 ") ;
   I := Get_The_Readonly_Attribute(Myobj) ;
   Put_Line("Got the readonly_attribute -> " & Corba.Short'Image(I)) ;
   Put_Line("") ;

   Put_Line("void_proc ... ") ;
   Void_Proc(Myobj) ;
   Put_Line("void_proc -> OK ") ;
   Put_Line("") ;

   Put_Line("in_proc(1,2,3)") ;
   I := 1 ;
   J := 2 ;
   K := 3 ;
   In_Proc(Myobj, I, J, K) ;
   Put_Line("in_proc -> OK") ;
   Put_Line("") ;

   Put_Line("out_proc : expecting 10, 11, 12") ;
   Out_Proc(Myobj, I, J, K) ;
   Put_Line("out_proc : "
            & Corba.Short'Image(I)
            & ", "
            & Corba.Short'Image(J)
            & ", "
            & Corba.Short'Image(K)) ;
   Put_Line("") ;

   Put_Line("inout_proc : expecting 3, 4") ;
   I := 2 ;
   J := 3 ;
   Inout_Proc(Myobj, I, J) ;
   Put_Line("inout_proc : "
            & Corba.Short'Image(I)
            & ", "
            & Corba.Short'Image(J)) ;
   Put_Line("") ;

   Put_Line("in_out_proc (1,2), expecting 3,4") ;
   In_Out_Proc(Myobj, I,J,K,L) ;
   Put_Line("in_out_proc "
            & Corba.Short'Image(K)
            & ", "
            & Corba.Short'Image(L)) ;
   Put_Line("") ;

   Put_Line("in_inout_proc(-1,-2,-3,-4), expecting -1, 36, -3, 40") ;
   I := -1 ;
   J := -2 ;
   K := -3 ;
   L := -4 ;
   In_Inout_Proc(Myobj, I,J,K,L) ;
   Put_Line("in_inout_proc -> OK"
            & Corba.Short'Image(i)
            & ", "
            & Corba.Short'Image(j)
            & ", "
            & Corba.Short'Image(K)
            & ", "
            & Corba.Short'Image(L)) ;
   Put_Line("") ;

   Put_Line("out_inout_proc(-11,-21,-31,-41), expecting 45,46,47,48") ;
   I := -11 ;
   J := -21 ;
   K := -31 ;
   K := -41 ;
   Out_Inout_Proc(Myobj, I,J,K,L) ;
   Put_Line("out_inout_proc ->  "
               & Corba.Short'Image(i)
               & ", "
               & Corba.Short'Image(j)
               & ", "
               & Corba.Short'Image(k)
               & ", "
               & Corba.Short'Image(l)) ;
   Put_Line("") ;

   Put_Line("in_out_inout_proc, expecting 78, -54 ,81") ;
   I := 78 ;
   J := 79 ;
   K := 80 ;
   In_Out_Inout_Proc(Myobj, I,J,K) ;
   Put_Line("in_out_inout_proc -> "
               & Corba.Short'Image(i)
               & ", "
               & Corba.Short'Image(j)
               & ", "
               & Corba.Short'Image(k)) ;
   Put_Line("") ;

   Put_Line("void_fun ... expecting 3 ") ;
   I:= 0 ;
   I := Void_Fun(Myobj) ;
   Put_Line("void_fun  " & Short'Image(I)) ;
   Put_Line("") ;


   Put_Line("in_fun(1,2,3), expecting 7") ;
   I := 1 ;
   J := 2 ;
   K := 3 ;
   L := In_Fun(Myobj, I, J, K) ;
   Put_Line("in_fun -> " & Short'Image(L)) ;
   Put_Line("") ;

   Put_Line("out_fun : expecting 5,6,7,10") ;
   Out_Fun(Myobj, I, J, K, L) ;
   Put_Line("out_fun : "
            & Corba.Short'Image(I)
            & ", "
            & Corba.Short'Image(J)
            & ", "
            & Corba.Short'Image(K)
            & ", "
            & Corba.Short'Image(L)) ;
   Put_Line("") ;

   Put_Line("inout_fun : expecting 3, 4, 7") ;
   I := 2 ;
   J := 3 ;
   Inout_Fun(Myobj, I, J, L) ;
   Put_Line("inout_fun : "
            & Corba.Short'Image(I)
            & ", "
            & Corba.Short'Image(j)
            & ", "
            & Corba.Short'Image(l)) ;
   Put_Line("") ;

   Put_Line("in_out_fun, expecting 11, 10, 21") ;
   I := 10 ;
   J := 11 ;
   In_Out_Fun(Myobj, I,J,K,L, M) ;
   Put_Line("in_out_fun -> "
            & Corba.Short'Image(I)
            & ", "
            & Corba.Short'Image(J)
            & ", "
            & Corba.Short'Image(M)) ;
   Put_Line("") ;

   Put_Line("in_inout_fun, expecting 53, 107, 12, 46, 153") ;
   I := 53 ;
   J := 54 ;
   K := 12 ;
   L := 34 ;
   In_Inout_Fun(Myobj, I,J,K,L,M) ;
   Put_Line("in_inout_fun -> "
            & Corba.Short'Image(I)
            & ", "
            & Corba.Short'Image(j)
            & ", "
            & Corba.Short'Image(k)
            & ", "
            & Corba.Short'Image(l)
            & ", "
            & Corba.Short'Image(M)) ;
   Put_Line("") ;

   Put_Line("out_inout_fun, expecting -86, -85, 86, 85, 1") ;
   J := -86 ;
   K := 85 ;
   Out_Inout_Fun(Myobj, I,J,K,L,M) ;
   Put_Line("out_inout_fun -> "
            & Corba.Short'Image(I)
            & ", "
            & Corba.Short'Image(j)
            & ", "
            & Corba.Short'Image(k)
            & ", "
            & Corba.Short'Image(l)
            & ", "
            & Corba.Short'Image(M)) ;
   Put_Line("") ;

   Put_Line("in_out_inout_fun, expecting 15, 16, -30, -1") ;
   I := 15 ;
   K := -45 ;
   In_Out_Inout_Fun(Myobj, I,J,K,L) ;
   Put_Line("in_out_inout_fun -> OK"
            & Corba.Short'Image(I)
            & ", "
            & Corba.Short'Image(j)
            & ", "
            & Corba.Short'Image(k)
            & ", "
            & Corba.Short'Image(l)) ;
   Put_Line("") ;


   Put_Line("oneway_void_proc ... ") ;
   Oneway_Void_Proc(Myobj) ;
   Put_Line("void_proc -> OK ") ;
   Put_Line("") ;

   Put_Line("onewayin_proc(1,2,3)") ;
   I := 1 ;
   J := 2 ;
   K := 3 ;
   Oneway_In_Proc(Myobj, I, J, K) ;
   Put_Line("in_proc -> OK") ;
   Put_Line("") ;


end Client ;




