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

	If you need further assitense or if you have questions, please contact us:
	dbcoretech.com (use our blog) or BtrieveFileSaver@dbcoretech.com

	______________________________________________________________________________________
	Change History:

	3rd August 2010 - C. Fiedler dbcoretech
	Changes to implement physical page stepping and including the 4096byte break at 
	6.x files.
	Prior to this change the lib was scanning the files by just reading binary data
	now you are jumping page wise accross the file and store the different page types.
	Thanks to Andrey Libowsky for the help on the 4096byte information. Only having this
	information you can jump on 6.x files by multiplying the page size. 

	11th August 2010 - N.Stagno dbcoretech
	Bug fix - reset GlobalCurPage variable.
	Prior to this change the tool would not be able to use multiple files in a loop because 
	the GlobalCurPage variable would not be reset to 0L while opening the new file.
	Check Line 317 (BF_OPEN)

	12. August 2010 - C. Fiedler dbcoretech
	Bug fix - read and/or extract data from files Version 5.x
	Prior to this change the tool would not find any data page on files version 5.x because 
	at the function getPageType the switch wasn't checking for BTRIEVE_FILE_V5
	Check Line 100 (getPageType)

	16th August 2010 - N.Stagno dbcoretech
	Bug fix - read and extract variable data from files Version 3.x/4.x/5.x/6.x
	Prior to the fix the function has scanned the traced VAR-Pages to find the right 
	page (using pageID). Now we use a function copied from Jim Kyles code (lp_pp)
	(Check getVariableData)

	18th August 2010 - C. Fiedler dbcoretech
	Bug fix reading files version 3.x/4.x/5.x and getting different results compared
	to BUTIL - SAVE.
	Prior to the fix the indication that a record is empty was incorrect. Now we scan 
	the record for any data other then 0x00 starting at the record adress + header
	(Check  BF_GET_REC). There are many other changes to the code so basically it is 
	a complete re-writing. we fixed:
			-	dont read pages where offset is behind the size of file
			-	dont read pages with offset 0x00 (that is allways the FCR)
			-	ignore records that are empty (V3,V4,V5)
			-	change DAT_HEADER_PAGE_V3 
			-	do not use pages that don't have a usage count
	many more minor changes too.
______________________________________________________________________________________

*/

#include "BtrieveFileSaverLib.h"

/*
*
*	Function freeClient
*
*	<input>	cl				CLIENT_STRUCT pointer
*	<input> ErrorCode		unsigned short int, error code that will be returned at exit
*
*	This function releases allocated resources and returns the given error code.
*	It resets the NumXXXPages to 0L just in case the pointer will be reused later on.
*
*/
unsigned short int freeClient (CLIENT_STRUCT *cl, unsigned short int ErrorCode)
{
	if (cl){
		if (cl->fHandle) fclose (cl->fHandle);
		if (cl->DATArr) free (cl->DATArr);
		if (cl->VATArr) free (cl->VATArr);
		if (cl->CUR_DPAGE) free (cl->CUR_DPAGE);
		if (cl->CUR_VPAGE) free(cl->CUR_VPAGE);
	}
	return ErrorCode;
}

/*
*
*	Function byte_swap
*	
*	<input/output>	i	4 byte integer to be byte-swapped
*
*	Just swap the bytes of a 32 bit int value
*
*/
static unsigned long int byte_swap(unsigned long int i )
{
	return ((i>>16)&0xFFFF) | (i<<16);
}

/*
*
*	Function getPageType
*
*	<input>	cl				CLIENT_STRUCT pointer 
*	<input> tmpPage			char pointer			// the page to be identified
*
*	This function is switching the supported versions and returns the type of page if identified. 
*	Sure you can do the same thing by using a struct union but for readability reason we did it 
*	this way.
*
*/
char	getPageType (CLIENT_STRUCT *cl, char *tmpPage)
{
	switch (cl->fVersion){
		case BTRIEVE_FILE_V3:
		case BTRIEVE_FILE_V4:
		case BTRIEVE_FILE_V5:{
			if (*(unsigned short int*)(tmpPage + 4) & 0x8000) return DAT_PAGE_ID;
		}break;
		case BTRIEVE_FILE_V6:
		case BTRIEVE_FILE_V61:
		case BTRIEVE_FILE_V7:{
			return tmpPage[1];
		}break;
		case BTRIEVE_FILE_V8:
		case BTRIEVE_FILE_V9:
//		case BTRIEVE_FILE_V95:
			{
			return tmpPage[4];
		}break;
	}
	return 0x00;	
}

/*
*
*	Function readPageFromFile
*
*	<input>			cl				CLIENT_STRUCT pointer 
*	<input/output>	tmpPage			char pointer holding the current page as container
*	<input>			pageOff			unsigned long page offset
*
*	Function to read a page based on it's offset within the file. 
*
*/
unsigned short int readPageFromFile (CLIENT_STRUCT *cl, void* tmpPage, unsigned long pageOff)
{
	unsigned long int retval = 0;
	if ((retval = fseek(cl->fHandle, pageOff, SEEK_SET)) == NO_ERROR){
		if ((fread(tmpPage, cl->fPageSize, 1, cl->fHandle)) == 1) return NO_ERROR;
	}
	return IO_ERROR;
}

/*
*
*	Function addPage
*
*	<input>	cl				CLIENT_STRUCT pointer 
*	<input>	tmpPage			char pointer holding the current page as container
*	<input>	pageType		char indicating the type of page
*
*	Function to add the page if the page type is either VAT_PAGE or DAT_PAGE. 
*
*/
unsigned long int	addPage (CLIENT_STRUCT *cl, char *tmpPage, char pageType)
{
	void				*pvTemp;
	unsigned long int	pCnt;
	unsigned long int	pId;
	unsigned short int	pUsage;
	unsigned long int	*elementCnt;
	unsigned long int	fOff = ftell(cl->fHandle) - cl->fPageSize;
	PAGE_LINK			*arr;

	if (pageType == DAT_PAGE_ID){
		arr = cl->DATArr;
		elementCnt = &cl->numDATPages;
	}else if (pageType == VAT_PAGE_ID){
		arr = cl->VATArr;
		elementCnt = &cl->numVATPages;
	}else return NO_ERROR;

	if (cl->fVersion >= BTRIEVE_FILE_V8){
		pId =  *(short*) tmpPage;
		pUsage = (short)*(tmpPage + 6);
	}else{
		pId = *(unsigned long int*)tmpPage;
		pId &= PAGE_KICK_OFF;
		pId = byte_swap(pId);
		pUsage = *(signed short*)(tmpPage + 4);
	}

	/* check if you have allready a page using the same id but lower usage counter */
	for (pCnt = 0; pCnt < *elementCnt; pCnt++){
		if (arr[pCnt].pId == pId && arr[pCnt].indexA < pUsage){
			/* replace existing page since it is a lower usage counter */ 
			arr[pCnt].offset = fOff;
			arr[pCnt].indexA = pUsage;
			return NO_ERROR;

		}else if (arr[pCnt].pId == pId){
			/* if the page allready exists and the one that allready exist has a higher usage, just exit */
			return NO_ERROR;
		}
	}

	/* add the page to the array */
	if ((pvTemp = (PAGE_LINK*) realloc (arr, ((*elementCnt) +1) * sizeof(PAGE_LINK))) == NULL) return MEM_ERROR;
	else{
		arr = (PAGE_LINK*)pvTemp;
		arr[*elementCnt].pId = pId;
		arr[*elementCnt].offset = fOff;
		arr[*elementCnt].indexA = pUsage;
		*elementCnt = (*elementCnt) +1;
	}
	switch (pageType){
		case DAT_PAGE_ID: cl->DATArr = arr; break;
		case VAT_PAGE_ID: cl->VATArr = arr; break;
	}
	return NO_ERROR;
}

/*
*
*	Function getNextPhysicalPage
*
*	<input>	cl				CLIENT_STRUCT pointer 
*	<input/output> tmpPage	char pointer holding the current page as container
*
*	Function to read the file until either the EOF is reached or another page is found. 
*	Note this function is changed from the original getNextPage function. The new version 
*	reads a file by its physical pages and returns the page type at the end or 0x00 if EOF.
*
*/
unsigned long int GlobalCurPage = 0L;	// global counter for the current page that is read 

char	getNextPhysicalPage (CLIENT_STRUCT *cl, char* tmpPage)
{
	unsigned long int	pageID;
	char				pType = 0x00;

	pageID = GlobalCurPage++;
	pageID = (pageID * cl->fPageSize);
	if (cl->fVersion >= BTRIEVE_FILE_V6 && cl->fVersion < BTRIEVE_FILE_V7){
		if (pageID >= (2 + 30 * (cl->numPointerPerPat + 2)) * cl->fPageSize)
			pageID += 4096;
	}

	/* now read the page into the CUR_DPAGE container */
	if ((readPageFromFile (cl, tmpPage, pageID)) != NO_ERROR) 
		return 0x00;
	pType = getPageType (cl, tmpPage);
	return (pType == 0x00)? MAX_SINGLE_INT_VAL : pType;
}

/*
*
*	Function readAllPagesFromFile 
*
*	<input>	cl				CLIENT_STRUCT pointer 
*
*	This function to scan the file for given pages. Note that we know of files that have 
*	holes not fitting to the page size! Therefore we are scanning in full page size and 
*	if a hole is indicated we use smaller steps. 
*
*/
unsigned short int readAllPagesFromFile  (CLIENT_STRUCT *cl)
{
	char				pType = 0x00;

	/* read page by page to check if there is a PAT page available */
	while ((pType = getNextPhysicalPage(cl, cl->CUR_DPAGE)) != 0x00)
		addPage (cl, cl->CUR_DPAGE, pType);
	/* reset the header of the data page container */
	*(unsigned long int*)cl->CUR_DPAGE = 0L;
	return NO_ERROR;
}

/*
*
*	Function sortPages
*	
*	<input/output>	in		CLIENT_STRUCT pointer, should be NULL and will be used to allocate memory 
*	<input>	pageType		char indicating the type of page
*
*	This function reorganizes all pages by sorting them based on pageId. 
*
*/
void	sortPages (CLIENT_STRUCT *cl, char pageType)
{
	unsigned long int	cnt1, cnt2;
	unsigned long int	low;
	unsigned long int	numElements;
	PAGE_LINK			tmp, *in;
	
	if (pageType == DAT_PAGE_ID){
		in = cl->DATArr;
		numElements = cl->numDATPages;
	}else if (pageType == VAT_PAGE_ID){
		in = cl->VATArr;
		numElements = cl->numVATPages;
	}else return;

	/* do a selection sort first to get all items in order */
	for (cnt1 = 0; cnt1 < numElements; cnt1++){
        low = cnt1;
		for(cnt2 = cnt1 +1; cnt2 < numElements; cnt2++){
			if(in[cnt2].pId < in[low].pId) 
				low = cnt2; 
        }
		memcpy ((void*)&tmp, (void*)&in[low], sizeof(PAGE_LINK));
		memcpy (&in[low], &in[cnt1], sizeof(PAGE_LINK));
		memcpy (&in[cnt1], &tmp, sizeof(PAGE_LINK));
	}
}

/*
*
*	Function BF_OPEN (Btrieve File ReadRecords)
*	
*	<input/output>	in				CLIENT_STRUCT pointer, should be NULL and will be used to allocate memory 
*	<input>			fName			char pointer containing the file that should be read
*
*	This function offers the entrance to the lib by opening a file. The file later can be referenced by its
*	cl pointer. This is similar to Btrieve where the position block is a concurrency storage. 
*
*/
unsigned short int	BF_OPEN (CLIENT_STRUCT *cl, char *fName)
{
	FCR	lcFCR;  /* local buffer to read the FCR */

	/* Reset the GlobalCurPage counter to 0L */
	GlobalCurPage = 0L;

	if (!cl) return CLIENT_CONNECTION_ERROR;
	else memset (cl, 0x00, sizeof(CLIENT_STRUCT));

	cl->curRecordId = MAX_LONG_INT_VAL;
	cl->curRecOff	= MAX_LONG_INT_VAL;
	cl->curDPageID	= 1; // never start reading pages below 1 (FCR)

	/* try to open the file */
	if ((cl->fHandle = fopen (fName, "rb")) == NULL) return IO_ERROR;

	/* read the first FCR page (located at the beginning of the file */
	if ((fread (&lcFCR, sizeof(FCR), 1, cl->fHandle)) != 1) return freeClient (cl, IO_ERROR);
	
	/* 
	*	check the file version and get basic parameter
	*/
	if (lcFCR.PageId == 0x00000000){ // <= 5x Format
		cl->fVersion		= (short)abs (lcFCR.Version);
		cl->recHeaderSize	= 0;

	}else if (lcFCR.PageId == FCR_IDENT ){			
		cl->fVersion = (short)abs (lcFCR.NewFileVersion);
		cl->recHeaderSize = 2;

	}else return freeClient (cl, IO_ERROR);

	cl->IFixedRecLen	= lcFCR.IFixedRecordLength;
	cl->FixRecLen		= lcFCR.FixedRecordLength;
	cl->VarRecsAllowed	= lcFCR.VarRecsAllowed;

	if ((cl->fVersion != BTRIEVE_FILE_V3) &&
		(cl->fVersion != BTRIEVE_FILE_V4) &&
		(cl->fVersion != BTRIEVE_FILE_V5) &&
		(cl->fVersion != BTRIEVE_FILE_V6) &&
		(cl->fVersion != BTRIEVE_FILE_V61) && 
		(cl->fVersion != BTRIEVE_FILE_V7) &&
		(cl->fVersion != BTRIEVE_FILE_V8) &&
		(cl->fVersion != BTRIEVE_FILE_V9) /*&&
		(cl->fVersion != BTRIEVE_FILE_V95)*/) return freeClient (cl, NO_BTR_FILE);

	if (cl->fVersion > BTRIEVE_FILE_V5){
		FCR	lcFCR_shadow;  
		if (cl->fVersion >= BTRIEVE_FILE_V8){
			cl->fPageSize	= (lcFCR.PageSizeV8 * PAGE_SIZE_MULTIPLIER);
			cl->VFragParam	= VFRAG_CUT_V8;
		}else{ 
			cl->fPageSize	= lcFCR.PageSize;
			cl->VFragParam	= VFRAG_CUT_V6;
		}
		if (fseek (cl->fHandle, cl->fPageSize, SEEK_SET) != NO_ERROR) return IO_ERROR;
		/* read the shadow page to validate both pairs and retrieve the number of records */
		if ((fread (&lcFCR_shadow, sizeof(FCR), 1, cl->fHandle)) != 1) return freeClient (cl, IO_ERROR);
		if (lcFCR.FCRUsageCount > lcFCR_shadow.FCRUsageCount)
			cl->numRecs = byte_swap (*((unsigned long*)&lcFCR.numRecs));
		else
			cl->numRecs = byte_swap (*((unsigned long*)&lcFCR_shadow.numRecs));
	}else{ /* less then 6x format */ 
		cl->fPageSize	= lcFCR.PageSize;
		cl->VFragParam	= VFRAG_CUT_V5;
		cl->numRecs		= byte_swap (*((unsigned long*)&lcFCR.numRecs));
	}

	if (cl->fVersion >= BTRIEVE_FILE_V6 && cl->fVersion < BTRIEVE_FILE_V7){
		cl->numPointerPerPat	= (cl->fPageSize - PAT_PAGE_HEADER_SIZE_V6) / PAT_POINTER_SIZE_V6;
	}

	/* allocate memory to hold the current data page */
	if ((cl->CUR_DPAGE = (char*) malloc (cl->fPageSize)) == NULL) return IO_ERROR;
	else *(long*)cl->CUR_DPAGE = 0L;

	/* get the max number of pages */
	fseek (cl->fHandle, 0, SEEK_END);
	cl->fNumPages = (ftell (cl->fHandle) / cl->fPageSize);
	fseek (cl->fHandle, 0, SEEK_SET);

	/* scan the file for existing pages */
	readAllPagesFromFile (cl);
	sortPages (cl, DAT_PAGE_ID);
		
	/* if var-rec's, allocate memory to hold the current var-data page */
	if (cl->VarRecsAllowed){
		if ((cl->CUR_VPAGE = (char*) malloc (cl->fPageSize)) == NULL) return IO_ERROR;
		else *(long*)cl->CUR_VPAGE = 0L;
	}

	return NO_ERROR;
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *\
 *                                                           *
 *      This procedure converts a Logical Page number to     *
 *      a file offset, using the PAT for new format files.   *
 *                                                           *
\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
static long int lp_pp(CLIENT_STRUCT *cl, long int lp )
{ 
	unsigned long int	ret, pat1, pat2;
	unsigned short int	u1, u2, pppat;

	if (cl->fVersion > BTRIEVE_FILE_V5){
		ret = 2;                               // first PAT pair on 2, 3
		pppat = (cl->fPageSize >> 2) - 2;      // pages per PAT

		while( lp > pppat ){				   // off current page 
			lp -= pppat;					   //   so tally down index
			ret += (cl->fPageSize >> 2);       //   up the PAT pp nbr
		}

		pat1 = ret * (long)cl->fPageSize;      // first PAT of pair
		pat2 = pat1 + (long)cl->fPageSize;     // second right after it
		fseek( cl->fHandle, pat1+4L, 0 );      // get both usage counts
		fread( &u1, 2, 1, cl->fHandle );
		fseek( cl->fHandle, pat2+4L, 0 );
		fread( &u2, 2, 1, cl->fHandle );
		if( u1 > u2 )						   // choose most recent one
			ret = pat1;
		else
			ret = pat2;

		ret += (long)(( lp << 2 ) + 4L );	   // position in PAT
		fseek( cl->fHandle, ret, 0 );
		fread( &lp, 4, 1, cl->fHandle );       // read it into LP

		lp = byte_swap( lp );                  // and un-word-swap it
		lp &= 0xFFFFFFL;
	}

	if( lp == 0xFFFFFFL || lp == -1L )    // NULL pointer values
		ret = -1L;
	else                                  // convert to offset
		ret = lp * cl->fPageSize;
	return ret;
}

/*
*
*	Function getNextDataPage
*	
*	<input/output>	cl				CLIENT_STRUCT pointer	-	containing information about concurrency and ressources
*
*	Jump to the next available Data page and if non, return END_OF_FILE.
*
*/
unsigned short int getNextDataPage (CLIENT_STRUCT *cl)
{ 
	unsigned short int	retval		= NO_ERROR;
	unsigned long int pOff;
	unsigned long int cPId = cl->curDPageID;

	memset (cl->CUR_DPAGE, 0x00, cl->fPageSize);

	while ((getPageType (cl, cl->CUR_DPAGE)) != DAT_PAGE_ID /*&& cPId < cl->fNumPages-2*/){
		pOff = lp_pp (cl, cPId++);
	
		if (pOff > (cl->fNumPages * cl->fPageSize)) continue;

		if ((retval = readPageFromFile (cl, cl->CUR_DPAGE, pOff)) != NO_ERROR) return retval;

		if (cl->fVersion == BTRIEVE_FILE_V6 && *(unsigned short*)(cl->CUR_DPAGE+2) != cPId -1){
			*(unsigned long*)cl->CUR_DPAGE = 0L;
			continue;
		}

		if (cl->fVersion == BTRIEVE_FILE_V3){
			unsigned long usage = 0L;
			((unsigned char*)&usage)[2] = cl->CUR_DPAGE[4];
			((unsigned char*)&usage)[1] = cl->CUR_DPAGE[6];
			((unsigned char*)&usage)[0] = cl->CUR_DPAGE[7];
			if (usage == 0){
				memset (cl->CUR_DPAGE, 0x00 , 10);
				continue;
			}
		}
	}
	cl->curDPageID = cPId;
	return NO_ERROR;
}

/*
*
*	Function getVariableData
*	
*	<input/output>	cl				CLIENT_STRUCT pointer	-	containing information about concurrency and ressources
*	<output>		dataBuffer		char pointer			-	to retrieve the requested record data
*	<input/output>	dbLen			unsigned long pointer	-	length indicator
*
*	Copy the data of the current record and retrieve its variable portion.
*
*/
unsigned short int	getVariableData	(CLIENT_STRUCT *cl, char *dataBuffer, unsigned long int *dbLen, char * curRecAdr)
{
	unsigned short int	numFrag		= 0;
	unsigned short int	retval		= 0;
	unsigned long int	VPageId		= 0;
	unsigned short int	FragId		= 0;
	unsigned short int	FragOff		= 0;
	unsigned long int	pCnt		= 0L;
	unsigned long int	curLen		= cl->FixRecLen;
	unsigned long int	pPageOff	= 0L;


	/* copy fix data into the provided buffer */
	if (dataBuffer && *dbLen > 0)
		memcpy (dataBuffer, curRecAdr + cl->recHeaderSize, (cl->FixRecLen > *dbLen) ? *dbLen : cl->FixRecLen);

	if (cl->fVersion >= BTRIEVE_FILE_V8){
		VPageId = *(short*)(curRecAdr + cl->FixRecLen + cl->recHeaderSize);
		numFrag	= *(short*)(curRecAdr + cl->FixRecLen + 10);
	}else{
		((unsigned char*)&VPageId)[0] = * (curRecAdr + cl->recHeaderSize + curLen);
		((unsigned char*)&VPageId)[1] = 0x00;
		((unsigned char*)&VPageId)[2] = *(curRecAdr + cl->recHeaderSize + curLen + 1);
		((unsigned char*)&VPageId)[3] = *(curRecAdr + cl->recHeaderSize + curLen + 2);
		((unsigned char*)&numFrag)[0] = *(curRecAdr + cl->recHeaderSize + curLen + 3);
		if (VPageId != PAGE_KICK_OFF) VPageId = byte_swap (VPageId);
	}
	while (VPageId != PAGE_KICK_OFF && FragId != MAX_SHORT_INT_VAL){
			
		/* get the VAT page offset within the file */
		if (cl->fVersion >= BTRIEVE_FILE_V7){
			pPageOff = MAX_LONG_INT_VAL;
			for (pCnt = 0l; pCnt < cl->numVATPages; pCnt++){
				if (cl->VATArr[pCnt].pId == VPageId){
					pPageOff = cl->VATArr[pCnt].offset;				
					break;
				}
			}
		}else 
			pPageOff = lp_pp (cl, VPageId);

		if (pPageOff != MAX_LONG_INT_VAL || numFrag > 254){
			short int			fEId		= 0;
			unsigned short int	addLen		= 0;
			
			if ((fseek (cl->fHandle, pPageOff, SEEK_SET)) != NO_ERROR 
				|| (fread (cl->CUR_VPAGE, cl->fPageSize, 1, cl->fHandle)) != 1) return IO_ERROR; 

			FragId = ((cl->fPageSize -1) >> 1) - numFrag;
			FragOff = ((short*)cl->CUR_VPAGE)[FragId] & 0x7FFF;

			/* get to the end of the fragment */
			for(fEId = 1; ((short*)cl->CUR_VPAGE)[FragId - fEId] == -1; fEId++);
				
			/* calculate the length of the fragment */
			addLen = (((short*)cl->CUR_VPAGE)[ FragId - fEId] & 0x7FFF ) - FragOff - cl->VFragParam;
				
			if (addLen > cl->fPageSize * 2 || addLen == 0) break;

			if (cl->fVersion >= BTRIEVE_FILE_V8){
				VPageId = *(short*)(&((char*)cl->CUR_VPAGE)[ FragOff]);
				numFrag	= *(short*)(&((char*)cl->CUR_VPAGE)[ FragOff +4]);
			}else{
				((unsigned char*)&VPageId)[0] = * (cl->CUR_VPAGE + FragOff);
				((unsigned char*)&VPageId)[1] = 0x00;
				((unsigned char*)&VPageId)[2] = *(cl->CUR_VPAGE + FragOff + 1);
				((unsigned char*)&VPageId)[3] = *(cl->CUR_VPAGE + FragOff + 2);
				((unsigned char*)&numFrag)[0] = *(cl->CUR_VPAGE + FragOff + 3);

				if (VPageId != PAGE_KICK_OFF) VPageId = byte_swap (VPageId);

				if (cl->fVersion < BTRIEVE_FILE_V6){
					if (((short*)cl->CUR_VPAGE)[ FragId] & 0x8000){
						FragOff += sizeof (VRECPTR);
						addLen -= sizeof(VRECPTR);
					}else VPageId = PAGE_KICK_OFF;
				}
			}

			if ((*dbLen)-curLen  > 0){
				unsigned long int len = ((*dbLen) - curLen >= addLen )? addLen : (*dbLen) - curLen;
				memcpy (dataBuffer + curLen, ((char*)cl->CUR_VPAGE) + FragOff + cl->VFragParam, len); 
			}
			curLen += addLen;

		}else break;
	}
	if (*dbLen == 0 || *dbLen < curLen)
		retval = DATA_BUFFER_TO_SHORT;

	*dbLen = (unsigned short)curLen;
	return retval;
}

/*
*
*	Function getNextRecord
*	
*	<input/output>	cl				CLIENT_STRUCT pointer	-	containing information about concurrency and ressources
*
*	get the next available record pointer.
*
*/
signed short int getNextRecord	(CLIENT_STRUCT *cl)
{
	signed short int	retval		= NO_ERROR;
	unsigned long int	numRecs		= ((cl->fPageSize -2) / cl->IFixedRecLen);

	if (cl->curRecordId  != MAX_LONG_INT_VAL && cl->curRecordId >= cl->numRecs-1) return END_OF_FILE;
	if (numRecs > (cl->curRecOff)){
		cl->curRecAdr = (((char*)cl->CUR_DPAGE) + ((cl->fVersion >= BTRIEVE_FILE_V8)? DAT_PAGE_HEADER_SIZE_V8 : DAT_PAGE_HEADER_SIZE_V6) 
							+ (cl->curRecOff * cl->IFixedRecLen));
		cl->curRecOff++;
	}else{
		if ((retval = getNextDataPage (cl)) != NO_ERROR) return retval;
		else{
			cl->curRecAdr = (((char*)cl->CUR_DPAGE)	+ ((cl->fVersion >= BTRIEVE_FILE_V8)? DAT_PAGE_HEADER_SIZE_V8 : DAT_PAGE_HEADER_SIZE_V6));
			cl->curRecOff = 1;
		}
	}
	return retval;
}

/*
*
*	Function BF_GET_REC (Btrieve File Get Next Record)
*	
*	<input/output>	cl				CLIENT_STRUCT pointer	-	containing information about concurrency and ressources
*	<output>		dataBuffer		char pointer			-	to retrieve the requested records
*	<input/output>	dbLen			unsigned long pointer	-	length indicator
*
*	Afer a client has opened a file it can start reading the file. To do so he calls BF_GET_REC to retrieve all records
*	based on their physical location within the file. The function returns EOF if no more records are available. 
*	Note that there is no such function like step first. This is because BF_GET_REC recognizes if your are at the 
*	beginning of a file.
*
*/
unsigned short int	BF_GET_REC (CLIENT_STRUCT *cl, char *dataBuffer, unsigned long *dbLen)
{
	signed short int	retval		= NO_ERROR;

	if (!cl) return CLIENT_CONNECTION_ERROR;

	if (cl->numRecs == 0L) return END_OF_FILE;
	while ((retval = getNextRecord (cl)) == NO_ERROR){
		if (cl->fVersion >= BTRIEVE_FILE_V6 && *(unsigned short*)(cl->curRecAdr) != 0x0000){
			break;
		}else if (cl->fVersion < BTRIEVE_FILE_V6){
			unsigned long int	i		= 0;
			char				isEmpty	= true;

			if (cl->fVersion == BTRIEVE_FILE_V3 && *(unsigned short int*)cl->curRecAdr == 0x0000) continue;

			for (i = ((cl->fVersion >= BTRIEVE_FILE_V4)? RECORD_HEADER_SIZE_V4 : RECORD_HEADER_SIZE_V3) ; 
					i < cl->IFixedRecLen ; i++){ 
				if (cl->curRecAdr[i]){
					isEmpty = false;
					break;
				}
			}

			if ((isEmpty == true)) continue;
			else break;
		}
	}

	if (retval != NO_ERROR) return retval;

	cl->curRecordId ++;

	if (cl->VarRecsAllowed){
		retval = getVariableData (cl, dataBuffer, dbLen, cl->curRecAdr);
	}else{
		if (dataBuffer && *dbLen > 0)
			memcpy (dataBuffer, cl->curRecAdr + cl->recHeaderSize, (cl->FixRecLen > *dbLen) ? *dbLen : cl->FixRecLen);
		if (*dbLen == 0 || *dbLen < cl->FixRecLen){
			retval = DATA_BUFFER_TO_SHORT;
		}
		*dbLen = cl->FixRecLen;
	}
	return retval;
}

/*
*
*	Function BF_CLOSE (Btrieve File Close)
*	
*	<input/output>	in				CLIENT_STRUCT pointer
*
*	This function releases all resources loaded by the client. 
*
*/
unsigned short int	BF_CLOSE (CLIENT_STRUCT *cl)
{
	if (!cl) return CLIENT_CONNECTION_ERROR;
	return freeClient (cl, NO_ERROR);
}
