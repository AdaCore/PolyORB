

#include <iostream.h>
#include "dominante.hh"
#include "orbTools.H"
#include <stdio.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>



int main (int argc, char **argv) 
{


        CORBA::ORB_ptr orb = CORBA::ORB_init(argc,argv,"omniORB2");


	if(argc!=2)
		PANIC(FALSE,"utilsation = client nom-fichier-mpeg\n");

  try {

	//
	//  On invoque le service de nom
	//    
	CosNaming::Name name;
	name.length(2);
    
	name[0].id   = (const char*) "mpeg";   
	name[0].kind = (const char*) "context"; 
	name[1].id   = (const char*) "dominante";
	name[1].kind = (const char*) "Object";
    
	CORBA::Object_var  obj = getObjectReference(orb,name);
    

	mpeg::mpegDecoder_var mp = mpeg::mpegDecoder::_narrow(obj);
	if (CORBA::is_nil(mp))
		PANIC(FALSE,"PB mp == nil\n");    


	int fd;
	if((fd = open(argv[1], O_RDONLY, 0 )) < 0)
               PANIC(TRUE,"Pb ouverture fichier\n");



	while(1)
		{

		mpeg::mpegFrame frame;

		int diag=read(fd,frame,mpeg::frameSize);
		
		if(diag<=0)
			{
			printf("client.C : fin du film\n");
			exit(0);
			}

		mp->putFrame(frame);


		}
   }
  CATCHORB;


return 0; 
}



