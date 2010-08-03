/* 

	Copyright (C)2010 dbcoretech ltd. <BtrieveFileSaver@dbcoretech.com>

	This file is part of BtrieveFileSaver.

    BtrieveFileSaver is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    BtrieveFileSaver is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BtrieveFileSaver.  If not, see <http://www.gnu.org/licenses/>.

*/
#pragma once

#ifdef _WIN32
	#define WIN32_LEAN_AND_MEAN

	#include <windows.h>
	/* Disable unsafe function warning */
	#pragma warning( disable : 4996)
#else
	#define NO_ERROR 0L
	#include <string.h>
#endif 

#include <stdio.h>
#include <stdlib.h>

#pragma pack(push)
#pragma pack(1)

/* 
*	Error Codes used in the project. Note that those are not the Btrieve Error codes! 
*	The main reason for using own error codes are just the limitations within the Btrieve 
*	codes.
*/
#define IO_ERROR                        2
#define MEM_ERROR						3
#define CLIENT_CONNECTION_ERROR			4		// there was a call without an established client struct
#define END_OF_FILE						5
#define DATA_BUFFER_TO_SHORT			6
#define NO_BTR_FILE						7

/* project defines */
#define MAX_SINGLE_INT_VAL					0xFF		// single byte int max value
#define MAX_SHORT_INT_VAL					0xFFFF		// 2 byte int max value
#define	MAX_LONG_INT_VAL					0xFFFFFFFF	// 4 byte int max value

#define PAGE_KICK_OFF						0xFFFF00FF	// define to overcome the page id at three byte page header

#define	FCR_IDENT							0x00004346	// byte swaped 'FC'


/* File type/feature flags */
#define FTYPE_NEW_FORMAT					0x01		// new file type indicated by 'FC' as FCR page id
#define FTYPE_VAR_REC						0x02		// variable rec len
#define FTYPE_BLANC_TRANC					0x04		// blank tranc
#define FTYPE_COMPRESSION					0x08		// compressed rec
#define	FTYPE_COMPRESSED_VAR				0x10		// compressed variable rec 
#define FTYPE_VAT_SUPPORT					0x20		// use VAT pages

/*	File version identifier */
#define BTRIEVE_FILE_V3						0x0300		// Btrieve 6.0 format
#define BTRIEVE_FILE_V4						0x0400		// Btrieve 6.0 format
#define BTRIEVE_FILE_V5						0x0500		// Btrieve 6.0 format
#define BTRIEVE_FILE_V6						0x0600		// Btrieve 6.0 format
#define BTRIEVE_FILE_V61					0x0610		// Btrieve 6.1 format
#define BTRIEVE_FILE_V7						0x0700		// Btrieve 7.0 format
#define BTRIEVE_FILE_V8						0x0800		// Btrieve 8.0 format
#define BTRIEVE_FILE_V9						0x0900		// Btrieve 9.0 format
//#define BTRIEVE_FILE_V95					0x0950		// Btrieve 9.5 format

/* Supported files FCR flags */
#define FCR_FLAG_VAR_REC					0x0001
#define FCR_COMPRESSION						0x0008
#define FCR_BLOBS							0x0800


#define DAT_PAGE_HEADER_SIZE_V6				2
#define DAT_PAGE_HEADER_SIZE_V8				4

#define PAT_PAGE_HEADER_SIZE_V6				8

/* VREC defines */
#define VFRAG_CUT_V5						0
#define VFRAG_CUT_V6						4
#define VFRAG_CUT_V8						6

/* RECORD POINTER SIZE */
#define DAT_POINTER_SIZE_V6					4
#define DAT_POINTER_SIZE_V8					6

#define PAT_POINTER_SIZE_V6					4

/* Page identifier */
#define FCR_PAGE_ID							'F'
#define PAT_PAGE_ID							'P'
#define	DAT_PAGE_ID							'D'
#define VAT_PAGE_ID							'V'

/* page size related defines */
#define PAGE_SIZE_MULTIPLIER				256			
#define PAGE_SIZE_STEPPING					512			


/* macro definition */
#define VRPage(x)	(long) (((long) (x).hi << 16) | ((x).mid << 8) | (x).lo)

/* struct defines */
typedef struct
{					/* (hi << 8 ) + lo = page */
	unsigned char	hi;
	unsigned char	lo;
	unsigned char	mid;
	unsigned char	frag;
} VRECPTR;

typedef struct{
	unsigned long int		pId;			/* page id, note that it is 16Bit */
	unsigned long int		offset;			/* physical position within the file */
	unsigned short int		indexA;			/* sort order based on pID */
}PAGE_LINK;

typedef struct{
	unsigned long int			PageId;
	unsigned short int			FCRUsageCount;
	short int					Version;
	short int					PageSize;
	char						buffer1[12];
	unsigned short int			FixedRecordLength;      /* 0  means compressed */
	unsigned short int			IFixedRecordLength;     /* rec length + overhead */
//  001Ah
	char						numRecs[4];				/* number of records byte swaped */
	char						buffer2[13];
	short int					PageSizeV8;				/* in V8 files indication of page size (multiplied by 256) */
	char						buffer3[11];
	char						VarRecsAllowed;			
	char						buffer4[17];
//  004Ah
	short int					NewFileVersion;
	char						buffer5[186];
	unsigned short int			UserFileFlags;          /* file flags as sent by user */
}FCR;

typedef struct{
	FILE					*fHandle;				/* handle to the physical file */
	short int				fVersion;				/* file version */
	unsigned short int		fPageSize;				/* the files page size */
	unsigned long int		numPointerPerPat;		/* counter of pointers per pat */
	unsigned long int		numRecs;				/* number of records as saved within the FCR */
	unsigned long int		curRecordId;			/* counter of records */
	char					VFragParam;				/* cut/add param for vrec fragments */
	char					VarRecsAllowed;			/* copy from FCR */
	unsigned short int		FixRecLen;				/* fixed rec len */
	unsigned short int		IFixedRecLen;			/* internal phys rec len */
	char					recHeaderSize;			/* header size of each rec */
	unsigned long int		curDPageID;				/* Id of the currently in use data page */
	char					*curRecAdr;				/* currently used record adrress*/
	unsigned long int		curRecOff;				/* Offset of the current record within the data page */
	/*
	*	data container - allocated ressources to hold the temporary used data pages
	*/
	char					*CUR_DPAGE;		/* The last used data page */
	char					*CUR_VPAGE;		/* variable data page buffer */
	/* 
	*	local page information 
	*	The following array's hold the information regarding the found pages.
	*	DATArr will be sorted by PageID - see BF_OPEN function
	*/
	unsigned long int		numVATPages;	/* number of vat pages within the file */
	PAGE_LINK				*VATArr;		/* array holding all vat pages */
	unsigned long int		numDATPages;	/* number of Dat pages within the file */
	PAGE_LINK				*DATArr;		/* array holding all dat pages */
}CLIENT_STRUCT;

/* Prototyping */
unsigned short int	BF_OPEN		(CLIENT_STRUCT *cl, char *fName);
unsigned short int	BF_GET_REC	(CLIENT_STRUCT *cl, char *dataBuffer, unsigned long *dbLen);
unsigned short int	BF_CLOSE	(CLIENT_STRUCT *cl);

#pragma pack(pop)