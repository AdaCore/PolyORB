/*
 * readfile.c --
 *
 *       Procedures concerned with reading data and parsing 
 *       start codes from MPEG files.
 *
 */

/*
 * Copyright (c) 1995 The Regents of the University of California.
 * All rights reserved.
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written agreement is
 * hereby granted, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

/*
 * Portions of this software Copyright (c) 1995 Brown University.
 * All rights reserved.
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written agreement
 * is hereby granted, provided that the above copyright notice and the
 * following two paragraphs appear in all copies of this software.
 * 
 * IN NO EVENT SHALL BROWN UNIVERSITY BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF BROWN
 * UNIVERSITY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * BROWN UNIVERSITY SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS ON AN "AS IS"
 * BASIS, AND BROWN UNIVERSITY HAS NO OBLIGATION TO PROVIDE MAINTENANCE,
 * SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

#include "../imageMpegLocal.H"
#include "video.h"
#include <sys/types.h>
#include <signal.h>
#ifndef MIPS
#include <netinet/in.h>
#else
#include <bsd/netinet/in.h>
#endif

#include "util.h"
#include "dither.h"

#ifdef __STDC__
#include <stdlib.h>
#include <string.h>
#endif

/*
   Changes to make the code reentrant:
      deglobalized: totNumFrames, realTimeStart, stream id vars, Prase_done,
         swap, seekValue, input, EOF_flag, ReadPacket statics, sys_layer,
	 bitOffset, bitLength, bitBuffer, curVidStream
   removed: [aud,sys,vid]Bytes
   Additional changes:
      get rid on ANSI C complaints about shifting
   -lsh@cs.brown.edu (Loring Holden)
 */



/* Silly Constants.... */
#define PACK_START_CODE             ((unsigned int)0x000001ba)
#define SYSTEM_HEADER_START_CODE    ((unsigned int)0x000001bb)
#define PACKET_START_CODE_MASK      ((unsigned int)0xffffff00)
#define PACKET_START_CODE_PREFIX    ((unsigned int)0x00000100)
#define ISO_11172_END_CODE          ((unsigned int)0x000001b9)
  
#define PACK_HEADER_SIZE 8
  
#define STD_AUDIO_STREAM_ID ((unsigned char) 0xb8)
#define STD_VIDEO_STREAM_ID ((unsigned char) 0xb9)
#define MIN_STREAM_ID_ID    ((unsigned char) 0xbc)
#define RESERVED_STREAM_ID  ((unsigned char) 0xbc)
#define PRIVATE_STREAM_1_ID ((unsigned char) 0xbd)
#define PADDING_STREAM_ID   ((unsigned char) 0xbe)
#define PRIVATE_STREAM_2_ID ((unsigned char) 0xbf)
  
#define STD_SYSTEM_CLOCK_FREQ (unsigned long)90000
#define MUX_RATE_SCALE_FACTOR 50
#define MAX_STREAMS 8
#define NOT_PACKET_ID       ((unsigned char) 0xff)
#define KILL_BUFFER         ((unsigned char) 0xfe)
  

/*
 *--------------------------------------------------------------
 *
 * get_more_data --
 *
 *	Called by get_more_data to read in more data from
 *      video MPG files (non-system-layer)
 *
 * Results:
 *	Input buffer updated, buffer length updated.
 *      Returns 1 if data read, 0 if EOF, -1 if error.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */
int 
imageMpegDecoder::get_more_data(
      VidStream *vid_stream)
{
  unsigned int **bs_ptr=&vid_stream->buf_start; 
  int *max_length=&vid_stream->max_buf_length;
  int *length_ptr=&vid_stream->buf_length;
  unsigned int **buf_ptr=&vid_stream->buffer;
  int ioBytes, data, result;
  unsigned char byte;
  unsigned int *mark;
  int sys_layer= vid_stream->sys_layer;
  
  if (sys_layer == 0) {
    return pure_get_more_data(*bs_ptr,
			      *max_length,
			       length_ptr,
			       buf_ptr,
			       vid_stream);
  }

  if (sys_layer == -1) {
    /* Time to init ourselves */
    vid_stream->swap = (htonl(1) != 1);
    mark = *bs_ptr;
    //ioBytes = fread(&data, 1, 4, vid_stream->input);
    localBuff->readBuffer((char*)(&data),4);
    ioBytes=4;

    if (ioBytes != 4) {
	  return 0;
    }

    data = ntohl(data);
    if ( (data == PACK_START_CODE) || (data == SYSTEM_HEADER_START_CODE) ) {
    got_sys:
      /* Yow, a System Layer Stream.  Much harder to parse.  Call in the
	     specialist.... */
      fprintf(stderr,"This is an MPEG System Layer Stream.  ");
      fprintf(stderr,"Audio is not played.\n");
      vid_stream->sys_layer = 1;
      result = read_sys(vid_stream,(unsigned int) data);
      return result;
    } else if (data ==  SEQ_START_CODE) {
    got_seq:
      /* No system Layer junk, just pretent we didn't peek,
	     and hereafter just call pure_get_more_data */
      vid_stream->sys_layer = 0;
      **bs_ptr = data;
      *length_ptr = 1;
      result = pure_get_more_data(*bs_ptr, *max_length, 
				 length_ptr, buf_ptr, vid_stream);
      *buf_ptr = *bs_ptr;
      return result;
    } else {
      int state;

      fprintf(stderr, "Junk at start of stream, searching for start code\n");
      state = 0;
      while (TRUE) {
	//if ((ioBytes = fread(&byte, 1, 1, vid_stream->input)) != 1) return 0;
	  localBuff->readBuffer((char*)(&byte),1);
	  ioBytes=1;
	if (byte == 0) {
	  if (state < 2) state++;
	} else if ((byte == 1) && (state == 2)) {
	  state++;
	} else {
	  state = 0;
	}
	if (state == 3) {
	  //if((ioBytes = fread(&byte, 1, 1, vid_stream->input)) != 1) return 0;
	  localBuff->readBuffer((char*)(&byte),1);
	  ioBytes=1;
	  data = ((unsigned int) byte + 0x100);
	  switch (data) {
	  case SEQ_START_CODE:
	    goto got_seq;
	  case PACK_START_CODE:
	  case SYSTEM_HEADER_START_CODE:
	    goto got_sys;
	  default:
	    /* keep looking */
	    state=0;
	  }
	}
      }}
  }

  /* A system layer stream (called after the 1st time), call the specialist */
  result = read_sys(vid_stream,0);
  return result;
}


/*
 *-------------------------------------------------------------
 *
 * clear_data_stream
 *
 * Empties out internal buffers
 *
 *-------------------------------------------------------------
 */
void 
  imageMpegDecoder::clear_data_stream(
      VidStream *vid_stream)
{
  /* Only internal buffer is in ReadPacket */
  if (vid_stream->sys_layer) {
    ReadPacket(KILL_BUFFER, vid_stream);
  }
}

/*
 *-------------------------------------------------------------
 *
 * SeekStream
 *
 * Goto an offset in the steam
 *
 *-------------------------------------------------------------
 */
void
  imageMpegDecoder::SeekStream(
      VidStream *vid_stream)
{
  int err;
  int code;
  
  if (vid_stream->seekValue < 0) return; /* done seeking */
#ifdef SEEK_SET
  err = fseek(vid_stream->input, vid_stream->seekValue, SEEK_SET);
#else
  err = fseek(vid_stream->input, vid_stream->seekValue, 0);
#endif
  if (err != 0) {
    fprintf(stderr,"Error in seek (%d)\n",err);
    perror("mpeg_play");
  }
  vid_stream->seekValue = 0-vid_stream->seekValue;
  vid_stream->totNumFrames = 0;

  /* clear that buffer */
  vid_stream->buffer = vid_stream->buf_start;
  vid_stream->buf_length = 0;
  vid_stream->bit_offset = 0;

  /* Find a decent start code */
 restart:
 NO_ZEROS:
  switch(fgetc(vid_stream->input)) {
  case 0:    goto ONE_ZERO;
  case EOF:  goto EOF_FOUND;
  default:   goto NO_ZEROS;
  }
  
 ONE_ZERO:
  switch(fgetc(vid_stream->input)) {
  case 0:    goto TWO_ZEROS;
  case EOF:  goto EOF_FOUND;
  default:   goto NO_ZEROS;
  }
  
 TWO_ZEROS:
  switch(fgetc(vid_stream->input)) {
  case 0x01:  goto CODE_FOUND;
  case 0x00:  goto TWO_ZEROS;
  case EOF:   goto EOF_FOUND;
  default:    goto NO_ZEROS;
  }
  
 CODE_FOUND:
  code = 0x00000100+fgetc(vid_stream->input);
  if (vid_stream->sys_layer) {
    clear_data_stream(vid_stream);
    if (((code & PACKET_START_CODE_MASK) == PACKET_START_CODE_PREFIX) &&
	((code & 0xff) >= 0xbc)) {
      read_sys(vid_stream, code);
      while (TRUE) {
	next_start_code(vid_stream);
	show_bits32(code);
	if ((code == SEQ_START_CODE) ||
	    (code == GOP_START_CODE)) return;
	flush_bits32; 
      }
    }
  } else {
    if ((code == SEQ_START_CODE) ||
	(code == GOP_START_CODE)) {
      *vid_stream->buffer = code;
      vid_stream->buf_length = 1;
      return;
    }
  }
  goto restart;

 EOF_FOUND:   /* received EOF */
  fprintf(stderr, "Hit EOF after seeking (offset %ld)\n",
     ftell(vid_stream->input));
  exit(1);

}


/*
 *--------------------------------------------------------------
 *
 * pure_get_more_data --
 *      (get_more_data from ver 2.0 with swap added)
 *
 *	Called by get_more_data to read in more data from
 *      video MPG files (non-system-layer)
 *
 * Results:
 *	Input buffer updated, buffer length updated.
 *      Returns 1 if data read, 0 if EOF, -1 if error.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

int 
imageMpegDecoder::pure_get_more_data(
     unsigned int *buf_start,
     int max_length,
     int *length_ptr,
     unsigned int **buf_ptr,
     VidStream *vid_stream)
{
  
  int length, num_read, i;
  unsigned int request;
  unsigned char *buffer, *mark;
  unsigned int *lmark;
  BOOLEAN swap=vid_stream->swap;
  
  if (vid_stream->EOF_flag) return 0;
  
  length = *length_ptr;
  buffer = (unsigned char *) *buf_ptr;
  
  if (length > 0) {
    memcpy((unsigned char *) buf_start, buffer, (unsigned int) (length*4));
    mark = ((unsigned char *) (buf_start + length));
  }
  else {
    mark = (unsigned char *) buf_start;
    length = 0;
  }
  
  request = (max_length-length)*4;
  
  
  //num_read = fread(mark, 1, request, vid_stream->input);
  localBuff->readBuffer((char*)mark,request);
  num_read=request;
  
  /* Paulo Villegas - 26/1/1993: Correction for 4-byte alignment */
  {
    int num_read_rounded;
    unsigned char *index;
    
    num_read_rounded = 4*(num_read/4);
    
    /* this can happen only if num_read<request; i.e. end of file reached */
    if ( num_read_rounded < num_read ) { 
 	  num_read_rounded = 4*( num_read/4+1 );

 	    /* fill in with zeros */
 	  for( index=mark+num_read; index<mark+num_read_rounded; *(index++)=0 );

 	  /* advance to the next 4-byte boundary */
 	  num_read = num_read_rounded;
    }
  }
  
  if (num_read < 0) {
    return -1;
  } else if (num_read == 0) {
    *buf_ptr = buf_start;
    
    /* Make 32 bits after end equal to 0 and 32
     * bits after that equal to seq end code
     * in order to prevent messy data from infinite
     * recursion.
     */
    
    *(buf_start + length) = 0x0;
    *(buf_start + length+1) = SEQ_END_CODE;
    
    vid_stream->EOF_flag = 1;
    return 0;
  }
  
  lmark = (unsigned int *) mark;
  
  num_read = num_read/4;
  
  if (swap) {
    for (i = 0; i < num_read; i++) {
      *lmark = htonl(*lmark);
      lmark++;
    }
  }
  
  *buf_ptr = buf_start;
  *length_ptr = length + num_read;
  
  return 1;
}




/* 
  Here is the specialist.... 
  Code is adapted from our program demux....
  A bunch of this needs to be #ifdef ANALYSIS'ed
  define __SYSREAD_LOGGING_ON__ to get  an output file for debugging
  */


/*
 *----------------------------------------------------------
 *
 *  read_sys
 *
 *      Parse out a packet of the system layer MPEG file.
 *
 *  Results:  Returns 0 if error or EOF
 *            Returns 1 if more data read (could be just one int)
 *
 *  Side Effects:  ReadPacket can change *bs_ptr to be a new buffer
 *                 buf_ptr will remain pointing at *length_ptr (at input)
 *                         into the buffer
 *                 *length_ptr will be changed to the new size
 *                 *max_length can be changed if a new buffer is alloc'd
 *
 *----------------------------------------------------------
 */
int imageMpegDecoder::read_sys(
     VidStream *vid_stream,
     unsigned int start)  
     /* start is either a start code or 0 to indicate continued parsing */
{
  unsigned int **bs_ptr=&vid_stream->buf_start;
  int *max_length = &vid_stream->max_buf_length;
  int *length_ptr=&vid_stream->buf_length;
  unsigned int **buf_ptr=&vid_stream->buffer;
  unsigned int startCode;
  int errorCode, PacketReply;
  unsigned char packetID;
  double systemClockTime;
  unsigned long muxRate;
  /* Statistics */
#ifdef ANALYSIS
  numPacks = 0;
  numPackets = 0;
  numSystemHeaders = 0;
#endif
  BOOLEAN match;
  
  if (!start) {
    errorCode = ReadStartCode(&startCode,vid_stream);
    if (vid_stream->EOF_flag) return 0;
    if (errorCode != 0) {
      fprintf(stderr, "Unable to read initial pack start code\n");
      return 0;
    }}
  else {
    errorCode = 0;
    startCode = start;
  }
  
  while (1) {
    match=FALSE;
    if (startCode == PACK_START_CODE) {
#ifdef ANALYSIS
      ++numPacks; 
#endif
      match = TRUE;
      errorCode = ReadPackHeader( &systemClockTime, &muxRate, vid_stream);
      if (errorCode != 0) {
        fprintf(stderr, "Error in reading pack header\n");
        return 0;
      }
      errorCode = ReadStartCode( &startCode, vid_stream );
      if (errorCode != 0) {
        fprintf(stderr, "Error in reading start code\n");
        return 0;
      }
    }
    if (startCode == SYSTEM_HEADER_START_CODE) {
#ifdef ANALYSIS
      ++numSystemHeaders; 
#endif
      match = TRUE;
      errorCode = ReadSystemHeader(vid_stream);
      if (errorCode != 0) {
        fprintf(stderr, "Error in reading system header\n");
        return 0;
      }
      errorCode = ReadStartCode( &startCode, vid_stream );
      if (errorCode != 0) {
        fprintf(stderr,"Error in reading start code after system header\n");
        return 0;
      }
    }
    packetID = startCode & 0xff;
    while (((startCode & PACKET_START_CODE_MASK) == PACKET_START_CODE_PREFIX) &&
	   (packetID >= 0xbc)) {
#ifdef ANALYSIS
      ++numPackets; 
#endif
      match = TRUE;
      packetID = startCode & 0xff;
      PacketReply = ReadPacket(packetID, vid_stream);
      switch (PacketReply) {
      case 2: 
        return 1;
      case 1: 
        return 0;
      default: /* do nothing */
        break;
      }
      errorCode = ReadStartCode( &startCode, vid_stream );
      if (errorCode != 0) {
        fprintf(stderr,"Error in start code after packet\n");
        return 0;
      }
      if (startCode == PACK_START_CODE || startCode == ISO_11172_END_CODE) {
        break;
      }
    }
    
    if (startCode == ISO_11172_END_CODE) {
      match = TRUE;
      if (vid_stream->Parse_done) {
	return 1;
      }
#ifdef ANALYSIS
      fprintf(stderr, "Successful parse of MPEG system level\n");
      fprintf(stderr, "%d system headers, %d packs, %d packets\n",
	      numSystemHeaders, numPacks, numPackets);
      fprintf(stderr, "%d audio packets, %d video packets, %d padding packets\n",
	      gNumAudioPackets, gNumVideoPackets, gNumPaddingPackets);
      fprintf(stderr, "%d reserved packets, %d/%d private type 1/2 packets\n",
	      gNumReservedPackets, gNumPrivate_1_Packets, gNumPrivate_2_Packets);
#endif
      ReadPacket(NOT_PACKET_ID, vid_stream);
      vid_stream->Parse_done = TRUE;
      return 1;
    }
    if (errorCode != 0)
      return 1;
    if (! match) {
      fprintf(stderr,"\nNo match found for start code %08x in system layer, skipping\n",startCode);
      startCode = find_start_code(vid_stream->input);
      if (startCode == EOF) {
        vid_stream->EOF_flag = 1;
        return 0;
      }
    }
  }
}


/*
 *-----------------------------------------------------------
 *
 *  ReadStartCode
 *
 *      Parses a start code out of the stream
 *
 *  Results/Side Effects:  Sets *startCode to the code, returns
 *     1 on error, 0 on success
 *
 *-----------------------------------------------------------
 */
int imageMpegDecoder::ReadStartCode(
     unsigned int *startCode,
     VidStream *vid_stream)
{
  int numRead;
  
  // numRead = fread((unsigned char *)startCode, 1, 4, vid_stream->input);
  localBuff->readBuffer((char*)startCode,4);
  numRead=4;
  *startCode = htonl(*startCode);
  
  if (numRead < 4) {
    vid_stream->EOF_flag = 1;
    return 1;
  }

  if ((*startCode&0xfffffe00) != 0) {
    fprintf(stderr,"Problem with system layer parse, skipping to start code\n");
    *startCode = find_start_code(vid_stream->input);
    if (*startCode == EOF) {
       vid_stream->EOF_flag = TRUE;
      return 0;
    }
  }

  return 0;
}


/*
 *-----------------------------------------------------------
 *
 *  find_start_code
 *
 *      Parses a start code out of the stream by tossing bytes until it gets one
 *
 *  Results/Side Effects:  Parses bytes of the stream, returns code
 *                         Returns EOF in case of end of file
 *
 *-----------------------------------------------------------
 */
int imageMpegDecoder::find_start_code(FILE* input)
{
 int deter;

 NO_ZEROS:
  localBuff->readBuffer((char*)(&deter),1);
  switch(deter) {
  case 0:    goto ONE_ZERO;
  case EOF:  goto EOF_FOUND;
  default:   goto NO_ZEROS;
  }

 ONE_ZERO:
  localBuff->readBuffer((char*)(&deter),1);
  switch(deter) {
  case 0:    goto TWO_ZEROS;
  case EOF:  goto EOF_FOUND;
  default:   goto NO_ZEROS;
  }

 TWO_ZEROS:
  localBuff->readBuffer((char*)(&deter),1);
  switch(deter) {
  case 0x01:  goto CODE_FOUND;
  case 0x00:  goto TWO_ZEROS;
  case EOF:  goto EOF_FOUND;
  default:    goto NO_ZEROS;
  }

 CODE_FOUND:
  localBuff->readBuffer((char*)(&deter),1);
  return 0x00000100+deter;

 EOF_FOUND:   /* received EOF */
  return EOF;
}




/*
 *-----------------------------------------------------------------
 *
 *  ReadPackHeader
 *
 *      Parses out the PACK header
 *
 *  Returns: 1 on error, 0 on success
 *
 *-------------------------------------------------------------------
 */
int imageMpegDecoder::ReadPackHeader(
     double *systemClockTime,
     unsigned long *muxRate,
     VidStream *vid_stream)
{
  int numRead;
  unsigned char inputBuffer[PACK_HEADER_SIZE];
  unsigned long systemClockRef;
  unsigned char systemClockRefHiBit;
  int errorCode;
  
  // numRead = fread(inputBuffer, 1, PACK_HEADER_SIZE, vid_stream->input);
  localBuff->readBuffer((char*)inputBuffer,PACK_HEADER_SIZE);
  numRead=PACK_HEADER_SIZE;
  if (numRead < PACK_HEADER_SIZE) {
    vid_stream->EOF_flag = 1;
    return 1;
  }
  ReadTimeStamp(inputBuffer, &systemClockRefHiBit, &systemClockRef);
  errorCode = MakeFloatClockTime(systemClockRefHiBit, systemClockRef, 
				 systemClockTime);
  ReadRate(&inputBuffer[5], muxRate);
  *muxRate *= MUX_RATE_SCALE_FACTOR;
  return 0;
}


/*
 *------------------------------------------------------------------
 *
 *   ReadSystemHeader
 *
 *      Parse out the system header, setup out stream IDs for parsing packets
 *
 *   Results:  Returns 1 on error, 0 on success.
 *             Sets gAudioStreamID and gVideoStreamID
 *
 *------------------------------------------------------------------
 */
int imageMpegDecoder::ReadSystemHeader(
   VidStream *vid_stream)
{ 
  unsigned char *inputBuffer = NULL;
  int numRead;
  int pos;
  unsigned short headerSize;
  unsigned char streamID;
  
  // numRead = fread((char *)&headerSize, 1, 2, vid_stream->input); 
  localBuff->readBuffer((char*)(&headerSize),2);
  numRead=2;
  headerSize = ntohs(headerSize);
  if (numRead != 2) {
    vid_stream->EOF_flag = 1;
    return 1;
  }
  inputBuffer = (unsigned char *) malloc((unsigned int) headerSize+1);
  if (inputBuffer == NULL) {
    return 1;
  }
  inputBuffer[headerSize]=0;
  // numRead = fread(inputBuffer, 1, headerSize, vid_stream->input); 
  localBuff->readBuffer((char*)inputBuffer,headerSize);
  numRead=headerSize;
  /* Brown - get rid of Ansi C complaints */
  if (numRead < (int) headerSize) {
    vid_stream->EOF_flag = 1;
    return 1;
  }
  
  pos = 6;
  while ((inputBuffer[pos] & 0x80) == 0x80) {
    streamID = inputBuffer[pos];
    switch (streamID) {
    case STD_VIDEO_STREAM_ID: 
      break;
    case STD_AUDIO_STREAM_ID: 
      break;
    case RESERVED_STREAM_ID: 
      break;
    case PADDING_STREAM_ID: 
      break;
    case PRIVATE_STREAM_1_ID: 
      break;
    case PRIVATE_STREAM_2_ID: 
      break;
    default:
      if (streamID < MIN_STREAM_ID_ID) {
	return 1;
      }
      switch (streamID >> 4) {
      case 0xc:
      case 0xd:
	vid_stream->gAudioStreamID = streamID;
	break;
      case 0xe:
	if ((vid_stream->gVideoStreamID != 0) &&
	    (vid_stream->gVideoStreamID!=streamID)) {
	  break;
	}
	vid_stream->gVideoStreamID = streamID;
	break;
      case 0xf:
/*Brown - deglobalized gReservedStreamID */
	vid_stream->gReservedStreamID = streamID;
	break;
      }
      break;
    }
    pos += 3;
  }
  if (inputBuffer != NULL)
    free(inputBuffer);
  return 0;
}


/*
 *-----------------------------------------------------------------
 *
 *  ReadPacket
 *
 *      Reads a single packet out of the stream, and puts it in the
 *      buffer if it is video.
 *
 *  Results:
 *      Changes the value of *length_ptr to be the new length (plus old)
 *      If the buffer is too small, can change *bs_ptr, *max_length, and 
 *      buf_ptr to be correct for a newly allocated buffer.
 *
 *  State:  
 *      The buffer is in ints, but the packets can be an arbitrary number
 *      of bytes, so leftover bytes are kept in the VidStream variable and
 *      are added on the next call.
 *
 *-----------------------------------------------------------------
 */   
int imageMpegDecoder::ReadPacket(unsigned char packetID, VidStream *vid_stream)

     /* Returns:
	0 - no error, but not video packet we want
	1 - error
	2 - got video packet into buffer
	*/
{   
  unsigned int **bs_ptr=&vid_stream->buf_start;
  int *max_length = &vid_stream->max_buf_length;
  int *length_ptr=&vid_stream->buf_length;
  unsigned int **buf_ptr=&vid_stream->buffer;
  int ioBytes;
  unsigned char nextByte;
  unsigned short packetLength;
  unsigned char *packetBuffer = NULL;
  int pos;
  int numStuffBytes = 0;
  unsigned int packetDataLength;
  int byte_length;
  unsigned char scratch[10];
  /* Leftovers from previous video packets */
  
  if (packetID == NOT_PACKET_ID) {
    /* Gross hack to handle unread bytes before end of stream */
    if (vid_stream->num_left != 0) {
      /* Sigh, deal with previous leftovers */
      *(*buf_ptr+*length_ptr) = vid_stream->leftover_bytes;
      *(*buf_ptr+*length_ptr+1) = ISO_11172_END_CODE;
      *length_ptr += 2;
    } else {
      *(*buf_ptr+*length_ptr) = ISO_11172_END_CODE;
      *length_ptr += 1;
    }
    return 1;
  } else if (packetID==KILL_BUFFER) {
    vid_stream->num_left=0;
    vid_stream->leftover_bytes=0;
    return 0;
  }
  
  ioBytes = fread(&packetLength, 1, 2, vid_stream->input);
  localBuff->readBuffer((char*)(&packetLength),2);
  ioBytes=2;
  packetLength = htons(packetLength);
  if (ioBytes < 2) {
    return 1;
  }
  if (packetID == vid_stream->gAudioStreamID) {
#ifdef ANALYSIS
    ++gNumAudioPackets;
#endif
  }
  else if (packetID == vid_stream->gVideoStreamID) {
#ifdef ANALYSIS     
    ++gNumVideoPackets;
#endif
  }
  else {
    switch (packetID) {
    case PADDING_STREAM_ID:
#ifdef ANALYSIS
      ++gNumPaddingPackets;
#endif
      break;
    case RESERVED_STREAM_ID:
#ifdef ANALYSIS
      ++gNumReservedPackets;
#endif
      break;
    case PRIVATE_STREAM_1_ID:
#ifdef ANALYSIS
      ++gNumPrivate_1_Packets;
#endif
      break;
    case PRIVATE_STREAM_2_ID:
#ifdef ANALYSIS
      ++gNumPrivate_2_Packets;
#endif
      break;
    default:
      fprintf(stderr, "\nUnknown packet type encountered. P'bly audio? (%x) \n",
//	      packetID,(int) ftell(vid_stream->input));
	      packetID);
    }
    if (packetID != vid_stream->gVideoStreamID) {/* changed by jim */
	fprintf(stderr,"Aye : fseek() retirer mais invoque !!!!\n");
      // fseek(vid_stream->input, packetLength, 1);
      return 0;
    }
  }

  // fread(&nextByte,1,1,vid_stream->input);
  localBuff->readBuffer((char*)(&nextByte),1);
  pos = 0;
  while (nextByte & 0x80) {
    ++numStuffBytes;
    ++pos;
   // fread(&nextByte,1,1,vid_stream->input);
  localBuff->readBuffer((char*)(&nextByte),1);
  }
  if ((nextByte >> 6) == 0x01) {
    pos += 2;
  localBuff->readBuffer((char*)(&nextByte),1);
  localBuff->readBuffer((char*)(&nextByte),1);
   // fread(&nextByte,1,1,vid_stream->input);
   // fread(&nextByte,1,1,vid_stream->input);
  } 
  if ((nextByte >> 4) == 0x02) {
    scratch[0] = nextByte;                      /* jim */
  //  fread(&scratch[1],1,4,vid_stream->input);   /* jim */
  localBuff->readBuffer((char*)(&scratch[1]),4);
  //  fread(&nextByte,1,1,vid_stream->input);
  localBuff->readBuffer((char*)(&nextByte),1);
    pos += 5;
  }
  else if ((nextByte >> 4) == 0x03) {
    scratch[0] = nextByte;                      /* jim */
  //  fread(&scratch[1],1,9,vid_stream->input);   /* jim */
  localBuff->readBuffer((char*)(&scratch[1]),9);
  //  fread(&nextByte,1,1,vid_stream->input);
  localBuff->readBuffer((char*)(&nextByte),1);
    pos += 10;
  } 
  else {
   // fread(&nextByte,1,1,vid_stream->input);
  localBuff->readBuffer((char*)(&nextByte),1);
    pos += 1;
  }
  /* Read all the headers, now make room for packet */
  if (*bs_ptr + *max_length < *buf_ptr+ packetLength/4 + *length_ptr) {
     /* Brown - get rid of Ansi C complaints */
    if (*max_length - *length_ptr < (int) packetLength/4) {
      /* Buffer too small for a packet (plus whats there), 
	   * time to enlarge it! 
	   */
      unsigned int *old = *bs_ptr;
      *max_length = *length_ptr + packetLength/2;
      *bs_ptr=(unsigned int *)malloc(*max_length*4);
      if (*bs_ptr == NULL) {
        return 1;
      }
      memcpy((unsigned char *)*bs_ptr, *buf_ptr, (unsigned int) *length_ptr*4);
      free(old);
      *buf_ptr = *bs_ptr;
    } else {
      memcpy((unsigned char *)*bs_ptr, *buf_ptr, (unsigned int) *length_ptr*4);
      *buf_ptr = *bs_ptr;
    }}
  byte_length = *length_ptr*4;
  if (vid_stream->num_left != 0) {
    /* Sigh, deal with previous leftovers */
    byte_length += vid_stream->num_left;
    *(*buf_ptr+*length_ptr) = vid_stream->leftover_bytes;
  }
  packetBuffer=((unsigned char *)*buf_ptr)+byte_length;
  packetDataLength = packetLength - pos;
  *packetBuffer++ = nextByte;
/* Brown - deglobalize gVideoStreamID */
  if (packetID == vid_stream->gVideoStreamID) {
    // ioBytes = fread(packetBuffer, 1, packetDataLength-1, vid_stream->input);
    localBuff->readBuffer((char*)packetBuffer,packetDataLength-1);
    ioBytes=packetDataLength-1;
    if (ioBytes != packetDataLength-1) {
      vid_stream->EOF_flag = 1;
      return 1;
    }
    if (1 != ntohl(1)) {
      unsigned int *mark = *buf_ptr+*length_ptr;
      int i;
      
      for (i=0; i < ((packetDataLength+
			 vid_stream->num_left)&0xfffffffc); i+=4) {
        *mark=ntohl(*mark);
        mark++;
      }
    }
    byte_length = byte_length + packetDataLength;
    vid_stream->num_left = byte_length % 4;
    *length_ptr = byte_length / 4;
    vid_stream->leftover_bytes = *(*buf_ptr + *length_ptr);
    return 2;
  }
  else if (packetID == vid_stream->gAudioStreamID) { 
    packetBuffer = (unsigned char *)(*buf_ptr + *length_ptr + 1);
    // fread(packetBuffer, 1, packetDataLength - 1, vid_stream->input);
    localBuff->readBuffer((char*)packetBuffer,packetDataLength-1);
  }
  else /* Donno what it is, just nuke it */ {
    /* This code should be unreachable */
    packetBuffer = (unsigned char *)(*buf_ptr + *length_ptr + 1);
    // fread(packetBuffer, 1, packetDataLength - 1, vid_stream->input);
    localBuff->readBuffer((char*)packetBuffer,packetDataLength-1);
  }
  return 0; 
}


/*
 * The remaining procedures are formatting utility procedures.
 */
void imageMpegDecoder::ReadTimeStamp(
     unsigned char *inputBuffer,
     unsigned char *hiBit,
     unsigned long *low4Bytes)
{
  *hiBit = ((unsigned long)inputBuffer[0] >> 3) & 0x01;
  *low4Bytes = (((unsigned long)inputBuffer[0] >> 1) & 0x03) << 30; 
  *low4Bytes |= (unsigned long)inputBuffer[1] << 22; 
  *low4Bytes |= ((unsigned long)inputBuffer[2] >> 1) << 15; 
  *low4Bytes |= (unsigned long)inputBuffer[3] << 7; 
  *low4Bytes |= ((unsigned long)inputBuffer[4]) >> 1; 
}

void imageMpegDecoder::ReadSTD(
unsigned char *inputBuffer,
unsigned char *stdBufferScale,
unsigned long *stdBufferSize)
{
  /* Brown - get rid of ANSI C complaints */
  *stdBufferScale = ((int)(inputBuffer[0] & 0x20) >> 5); 
  *stdBufferSize = ((unsigned long)inputBuffer[0] & 0x1f) << 8;
  *stdBufferSize |= (unsigned long)inputBuffer[1];
}


void imageMpegDecoder::ReadRate(
     unsigned char *inputBuffer,
     unsigned long *rate)
{
  *rate = (inputBuffer[0] & 0x7f) << 15;
  *rate |= inputBuffer[1] << 7;
  /* Brown - get rid of ANSI C complaints */
  *rate |= (int) (inputBuffer[2] & 0xfe) >> 1;
}

#define FLOAT_0x10000 (double)((unsigned long)1 << 16)

int imageMpegDecoder::MakeFloatClockTime(unsigned char hiBit, unsigned long low4Bytes,
		       double * floatClockTime)
{
  if (hiBit != 0 && hiBit != 1) {
    *floatClockTime = 0.0;
    return 1;
  }
  *floatClockTime 
    = (double)hiBit*FLOAT_0x10000*FLOAT_0x10000 + (double)low4Bytes;
  *floatClockTime /= (double)STD_SYSTEM_CLOCK_FREQ;
  return 0;
}
