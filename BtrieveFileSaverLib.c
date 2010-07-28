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
	unsigned short int	pCnt;
	unsigned short int	pId;
	unsigned short int	pUsage;
	unsigned long int	*elementCnt;
	PAGE_LINK			*arr;

/*	if (pageType == PAT_PAGE_ID){
		arr = cl->PATArr;
		elementCnt = &cl->numPATPages;
	}else*/ if (pageType == DAT_PAGE_ID){
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
		pId =  *(short*)(tmpPage +2);
		pUsage = *(tmpPage + 4);
	}

	/* check if you have allready a page using the same id but lower usage counter */
	for (pCnt = 0; pCnt < *elementCnt; pCnt++){
		if (arr[pCnt].pId == pId && arr[pCnt].indexA < pUsage){
			/* replace existing page since it is a lower usage counter */ 
			arr[pCnt].offset = (ftell (cl->fHandle)) - cl->fPageSize;
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
		arr[*elementCnt].offset = (ftell (cl->fHandle)) - cl->fPageSize;
		arr[*elementCnt].indexA = pUsage;
		*elementCnt = (*elementCnt) +1;
	}
	switch (pageType){
//		case PAT_PAGE_ID: cl->PATArr = arr; break;
		case DAT_PAGE_ID: cl->DATArr = arr; break;
		case VAT_PAGE_ID: cl->VATArr = arr; break;
	}
	return NO_ERROR;
}

/*
*
*	Function getNextPage
*
*	<input>	cl				CLIENT_STRUCT pointer 
*	<input/output> tmpPage	char pointer holding the current page as container
*
*	Function to read the file until either the EOF is reached or another page is found. 
*	Note that we know of files that have holes not fitting to the page size! 
*	Therefore we are scanning in full page size and if a hole is indicated we use 
*	smaller steps. 
*
*/
char	getNextPage (CLIENT_STRUCT *cl, char* tmpPage)
{
	char				pType = 0x00;
	unsigned long int	stepSize = cl->fPageSize;
	
	while ((fread(tmpPage, stepSize, 1, cl->fHandle)) == 1){
		if ( (cl->fVersion > BTRIEVE_FILE_V5 && *(short*)tmpPage != 0x0000 && (pType = getPageType (cl, tmpPage)) != 0x00)
			|| (cl->fVersion <= BTRIEVE_FILE_V5 && (pType = getPageType (cl, tmpPage)) != 0x00)){
			/* check if you have read the entire page or only PAGE_SIZE_STEPPING */
			if (stepSize == PAGE_SIZE_STEPPING){
				if (fseek (cl->fHandle, ((ftell(cl->fHandle)) - stepSize), SEEK_SET) != NO_ERROR) return 0x00;
				if ((fread(tmpPage, cl->fPageSize, 1, cl->fHandle)) != 1) return 0x00;
			}
			break;
		}else if (*(short*)tmpPage == 0x0000)
			stepSize = (cl->fVersion > BTRIEVE_FILE_V5)? PAGE_SIZE_STEPPING : cl->fPageSize;
	}
	return pType;
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
	char				*lcPPage = NULL;

	/* rewind the current position before allocating page to prevent unnecessary malloc/free */
	if ((fseek(cl->fHandle, 0L, SEEK_SET)) != NO_ERROR) return IO_ERROR;

	/* 
	*	allocate a local PAT_PAGE buffer 
	*/
	if ((lcPPage = (char*) malloc (cl->fPageSize)) == NULL) return MEM_ERROR;

	/* read page by page to check if there is a PAT page available */
	while ((pType = getNextPage(cl, lcPPage)) != 0x00)
		addPage (cl, lcPPage, pType);

	/* free up all allocated memory */
	free (lcPPage);
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
	unsigned short int	cnt1, cnt2;
	unsigned short int	low;
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
*	Function BF_OPEN (Btrieve File Open)
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

	if (!cl) return CLIENT_CONNECTION_ERROR;
	else memset (cl, 0x00, sizeof(CLIENT_STRUCT));

	/* try to open the file */
	if ((cl->fHandle = fopen (fName, "rb")) == NULL) return IO_ERROR;

	/* read the first FCR page (located at the beginning of the file */
	if ((fread (&lcFCR, sizeof(FCR), 1, cl->fHandle)) != 1) return freeClient (cl, IO_ERROR);
	
	/* 
	*	check the file version and get basic parameter
	*/
	if (lcFCR.PageId == 0x00000000){ // <= 5x Format
		cl->fVersion = (short)abs (lcFCR.Version);
		cl->recHeaderSize = 4;
	}else if (lcFCR.PageId == FCR_IDENT ){			
		cl->fVersion = (short)abs (lcFCR.NewFileVersion);
		cl->recHeaderSize = 6;
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
		if (cl->fVersion >= BTRIEVE_FILE_V8){
			cl->fPageSize = (lcFCR.PageSizeV8 * PAGE_SIZE_MULTIPLIER);
			cl->VFragParam = VFRAG_CUT_V8;
		}else{ 
			cl->fPageSize = lcFCR.PageSize;
			cl->VFragParam = VFRAG_CUT_V6;
		}
		if (fseek (cl->fHandle, cl->fPageSize, SEEK_SET) != NO_ERROR) return IO_ERROR;
	}else{ /* less then 6x format */ 
		cl->fPageSize = lcFCR.PageSize;
		cl->VFragParam = VFRAG_CUT_V5;
	}

	/* scan the file for existing pages */
	readAllPagesFromFile (cl);
	sortPages (cl, DAT_PAGE_ID);
		
	/* allocate memory to hold the current data page */
	if ((cl->CUR_DPAGE = (char*) malloc (cl->fPageSize)) == NULL) return IO_ERROR;
	else *(long*)cl->CUR_DPAGE = 0L;

	if ((cl->CUR_DPAGE = (char*) malloc (cl->fPageSize)) == NULL) return IO_ERROR;
	else *(long*)cl->CUR_DPAGE = 0L;

	/* if var-rec's, allocate memory to hold the current var-data page */
	if (cl->VarRecsAllowed){
		if ((cl->CUR_VPAGE = (char*) malloc (cl->fPageSize)) == NULL) return IO_ERROR;
		else *(long*)cl->CUR_VPAGE = 0L;
	}

	return NO_ERROR;
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

	if (cl->curDPageID >= cl->numDATPages) return END_OF_FILE;
	if ((retval = readPageFromFile (cl, cl->CUR_DPAGE, cl->DATArr[cl->curDPageID++].offset)) != NO_ERROR) 
			return retval;
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
	unsigned short int	VPageId		= 0;
	unsigned short int	FragId		= 0;
	unsigned short int	FragOff		= 0;
	unsigned long int	pCnt		= 0L;
	unsigned long int	curLen		= cl->FixRecLen;
	unsigned long int	pPageOff	= 0L;
	VRECPTR				Vrec;
		
	/* copy fix data into the provided buffer */
	if (dataBuffer && *dbLen > 0)
		memcpy (dataBuffer, curRecAdr + cl->recHeaderSize, (cl->FixRecLen > *dbLen) ? *dbLen : cl->FixRecLen);

	if (cl->fVersion >= BTRIEVE_FILE_V8){
		VPageId = *(short*)(curRecAdr + cl->FixRecLen + cl->recHeaderSize);
		numFrag	= *(short*)(curRecAdr + cl->FixRecLen + 10);
	}else{
		memcpy (&Vrec, curRecAdr + cl->recHeaderSize + curLen, sizeof(VRECPTR));
		VPageId = (short)VRPage(Vrec);
		numFrag	=  Vrec.frag;
	}
	while (VPageId != MAX_SHORT_INT_VAL && FragId != MAX_SHORT_INT_VAL){
			
		/* get the VAT page offset within the file */
		if (cl->fVersion >= BTRIEVE_FILE_V5){
			pPageOff = MAX_LONG_INT_VAL;
			for (pCnt = 0l; pCnt < cl->numVATPages; pCnt++){
				if (cl->VATArr[pCnt].pId == VPageId){
					pPageOff = cl->VATArr[pCnt].offset;				
					break;
				}
			}
		}else pPageOff = VPageId * cl->fPageSize;

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
				Vrec = *(VRECPTR *)(&((char*)cl->CUR_VPAGE)[ FragOff]);
				VPageId = (short)VRPage(Vrec);
				numFrag	=  Vrec.frag;
				if (cl->fVersion < BTRIEVE_FILE_V6){
					if (((short*)cl->CUR_VPAGE)[ FragId] & 0x8000){
						FragOff += sizeof (VRECPTR);
						addLen -= sizeof(VRECPTR);
					}else VPageId = MAX_SHORT_INT_VAL;
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
	signed short int		retval		= NO_ERROR;
	unsigned long int	numRecs		= 0;
	/*
	*	check if you can just use the next record or if you have to use a different 
	*	data page. Note that the curRecOff is 1 based.
	*/ 
	if (*(long*)cl->CUR_DPAGE != 0L && (numRecs = ((cl->fPageSize -2) / cl->IFixedRecLen)) > (cl->curRecOff)){
		/* 
		*	If there are more records at the current page, just jump to the next one
		*/
		cl->curRecAdr = (((char*)cl->CUR_DPAGE) 
							+ ((cl->fVersion >= BTRIEVE_FILE_V8)? DAT_PAGE_HEADER_SIZE_V8 : DAT_PAGE_HEADER_SIZE_V6) 
							+ (cl->curRecOff * cl->IFixedRecLen));
		cl->curRecOff++;
	}else{
		/* 
		*	The current page is at its end, try to load up the next page and if there isnt any, 
		*	return END_OF_FILE;
		*/
		if ((retval = getNextDataPage (cl)) != NO_ERROR) return retval;
		else{
			/* 
			*	since we found another data page, we can used the first available item
			*	and reset the curRecOff.
			*/
			cl->curRecAdr = (((char*)cl->CUR_DPAGE) 
								+ ((cl->fVersion >= BTRIEVE_FILE_V8)? DAT_PAGE_HEADER_SIZE_V8 : DAT_PAGE_HEADER_SIZE_V6));
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

	/* validate the client backpoint */
	if (!cl) return CLIENT_CONNECTION_ERROR;

	while ((retval = getNextRecord (cl)) == NO_ERROR){
		if (cl->fVersion >= BTRIEVE_FILE_V6 && *(short*)(cl->curRecAdr +4) != 0x0000){
			break;
		}else if (cl->fVersion < BTRIEVE_FILE_V6){
			  char *			tempPtr = cl->curRecAdr +4;
			  unsigned long int	i		= 0;

			  for (i = cl->IFixedRecLen - 4; i; i--) 
				  if (*tempPtr++) break;
			  if ((i == 0) && (( *((unsigned long int*) cl->curRecAdr) == 0) || ( *((unsigned long int*) cl->curRecAdr)  == MAX_LONG_INT_VAL)))
				 continue; 
			  else 
				  break;
		}
	}

	/* if any problem occours exit before going foreward */
	if (retval != NO_ERROR) return retval;

	cl->curRecordId ++;
	/* check if file uses variable rec or blanc trunc */
	if (cl->VarRecsAllowed){
		retval = getVariableData (cl, dataBuffer, dbLen, cl->curRecAdr);

	/* if the file uses compression or compressed variable rec's */
	/*}else if (cl->fType & FTYPE_COMPRESSION || cl->fType & FTYPE_COMPRESSED_VAR){
	*/
	}else{
		/* the record does not need any further treatment, copy it into the buffer and return to the caller */
	
		/* now copy the data into the provided buffer */
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
