#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "memlib.h"
#include "mm.h"

/* Basic constants and macros */
#define WSIZE 4 /* Word and header/footer size (bytes) */
#define DSIZE 8 /* Double word size (bytes) */
#define PSIZE 4
#define MINSIZE (2*WSIZE) + (2*PSIZE)
#define SIZEOFPLIST 20
#define CHUNKSIZE (1<<12) /* Extend heap by this amount (bytes) */
/*#define PSIZE sizeof(char*)
#define ISIZE sizeof(int)
#define LSIZE sizeof(long)
#define LLSIZE sizeof(long long)*/

#define MAX(x, y) ((x) > (y)? (x) : (y))

/* Pack a size and allocated bit into a word */
#define PACK(size, alloc) ((size) | (alloc))

/* Read and write a word at address p */
#define GET(p) (*(unsigned int *)(p))
#define PUT(p, val) (*(unsigned int *)(p) = (val))

/* Read the size and allocated fields from address p */
#define GET_SIZE(p) (GET(p) & ~0x7)
#define GET_ALLOC(p) (GET(p) & 0x1)

/* Given block ptr bp, compute address of its header and footer */
#define HDRP(bp) ((char *)(bp) - WSIZE)
#define FTRP(bp) ((char *)(bp) + GET_SIZE(HDRP(bp)) - DSIZE)

/* Given block ptr bp, compute address of next and previous blocks */
#define NEXT_BLKP(bp) ((char *)(bp) + GET_SIZE(((char *)(bp) - WSIZE)))
#define PREV_BLKP(bp) ((char *)(bp) - GET_SIZE(((char *)(bp) - DSIZE)))

#define NEXT_BLOCK(bp) (*(void **)(bp))
#define PRE_BLOCK(bp) (*(void **)(bp + PSIZE))

static void *heap_listp;
static void *free_listp[SIZEOFPLIST]; //FREE_LIST[0]: MINSIZE~, FREE_LIST[0]: MINSIZE<<1~, ...

int mm_init(void);
void* mm_malloc(size_t size);
void mm_free(void* bp);
void* mm_realloc(void* ptr, size_t size);

static void *extend_heap(size_t words);
static void *find_fit(size_t asize);
static void place(void *bp, size_t asize);
static void *coalesce(void *bp);
static void add_free(void *bp, size_t size);
static void delete_free(void *bp);

/*
 * mm_init - initialize the malloc package.
 * The return value should be -1 if there was a problem in performing the initialization, 0
 * otherwise
 */
int mm_init(void) {
    int list_i;
    /* Create the initial empty heap */
    if ((heap_listp = mem_sbrk((4 * WSIZE))) == (void *)-1)
        return -1;
    PUT(heap_listp, 0); /* Alignment padding */
    PUT(heap_listp + (1 * WSIZE), PACK(DSIZE, 1)); /* Prologue header */
    PUT(heap_listp + (2 * WSIZE), PACK(DSIZE, 1)); /* Prologue footer */
    //free_listp = heap_listp + (3 * WSIZE); /* FREE_LIST */

    /* init FREE_LIST */
    for (list_i = 0; list_i < SIZEOFPLIST; list_i++) {
        free_listp[list_i] = NULL;
    }

    PUT(heap_listp + ((3 * WSIZE)), PACK(0, 1)); /* Epilogue header */

    heap_listp += (2 * WSIZE);

    /* Extend the empty heap with a free block of CHUNKSIZE bytes */
    if (extend_heap(CHUNKSIZE/WSIZE) == NULL)
        return -1;
    return 0;
}

/*
 * mm_malloc - Allocate a block by incrementing the brk pointer.
 *     Always allocate a block whose size is a multiple of the alignment.
 */
void* mm_malloc(size_t size) {
    size_t asize; /* Adjusted block size */
    size_t extendsize; /* Amount to extend heap if no fit */
    char *bp;

    /* Ignore spurious requests */
    if (size == 0)
        return NULL;

    /* Adjust block size to include overhead and alignment reqs. */
    if (size <= 2*PSIZE)
        asize = MINSIZE;
    else
        asize = DSIZE * ((size + (DSIZE) + (DSIZE-1)) / DSIZE);

    /* Search the free list for a fit */
    if ((bp = find_fit(asize)) != NULL) {
        place(bp, asize);
        return bp;
    }

    /* No fit found. Get more memory and place the block */
    extendsize = MAX(asize,CHUNKSIZE);
    if ((bp = extend_heap(extendsize/WSIZE)) == NULL)
        return NULL;
    place(bp, asize);
    return bp;
}

/*
 * mm_free - Freeing a block does nothing.
 */
void mm_free(void* bp) {
    size_t size = GET_SIZE(HDRP(bp));
    
    PUT(HDRP(bp), PACK(size, 0));
    PUT(FTRP(bp), PACK(size, 0));
    coalesce(bp);
}

/*
 * mm_realloc
 */
void* mm_realloc(void* ptr, size_t size) {
    size_t asize;
    size_t csize;
    size_t nbsize;
    void *oldptr;
    void *newptr;

    /* edge case 1 */
    if (ptr == NULL) {
        return mm_malloc(size);
    }
    else {
        /* edge case 2 */
        if (size == 0) {
            mm_free(ptr);
            return NULL;
        }
        /* Adjust block size to include overhead and alignment reqs. */
        if (size <= 2*PSIZE)
            asize = MINSIZE;
        else
            asize = DSIZE * ((size + (DSIZE) + (DSIZE-1)) / DSIZE);

        csize = GET_SIZE(HDRP(ptr)); //current size
        nbsize = (GET_SIZE(HDRP(NEXT_BLKP(ptr))) * !GET_ALLOC(HDRP(NEXT_BLKP(ptr)))); //nextblock size

        /* cannot realloc in same ptr */
        if ((csize + nbsize) < asize) {
            oldptr = ptr;
            newptr = mm_malloc(size);
            memcpy(newptr, oldptr, csize);
            mm_free(oldptr);
            return newptr;
        }

        /* realloc in same ptr */
        /* case 1: use next free block */
        else if (csize < asize) {
            delete_free(NEXT_BLKP(ptr));
            if ((csize + nbsize) - asize < MINSIZE) { // too small scraps: cannot make new free block
                PUT(HDRP(ptr), PACK((csize + nbsize), 1));
                PUT(FTRP(ptr), PACK((csize + nbsize), 1));
            }
            else { // enough scraps: can make new free block
                PUT(HDRP(ptr), PACK(asize, 1));
                PUT(FTRP(ptr), PACK(asize, 1));
                PUT(HDRP(NEXT_BLKP(ptr)), PACK((csize + nbsize) - asize, 0));
                PUT(FTRP(NEXT_BLKP(ptr)), PACK((csize + nbsize) - asize, 0));
                add_free(NEXT_BLKP(ptr), (csize + nbsize) - asize);
            }

        }
        /* case 2: not use next free block */
        else {
            if (csize - asize < MINSIZE) { // not enough scraps: cannot make new free block
            }
            else { // enough scraps: can make new free block
            delete_free(NEXT_BLKP(ptr));
            PUT(HDRP(ptr), PACK(asize, 1));
            PUT(FTRP(ptr), PACK(asize, 1));
            PUT(HDRP(NEXT_BLKP(ptr)), PACK((csize - asize + nbsize), 0));
            PUT(FTRP(NEXT_BLKP(ptr)), PACK((csize - asize + nbsize), 0));
            add_free(NEXT_BLKP(ptr), (csize - asize + nbsize));
            }
        }
        return ptr;
    }
    return NULL; //WTF
}


static void *extend_heap(size_t words) {
    char *bp;
    size_t size;

    /* Allocate an even number of words to maintain alignment */
    size = (words % 2) ? (words+1) * WSIZE : words * WSIZE;
    if ((long)(bp = mem_sbrk(size)) == -1)
        return NULL;

    /* Initialize free block header/footer and the epilogue header */
    PUT(HDRP(bp), PACK(size, 0)); /* Free block header */
    PUT(FTRP(bp), PACK(size, 0)); /* Free block footer */

    PUT(HDRP(NEXT_BLKP(bp)), PACK(0, 1)); /* New epilogue header */

    /* Coalesce if the previous block was free */
    return coalesce(bp);
}

static void *coalesce(void *bp) {
    size_t prev_alloc = GET_ALLOC(FTRP(PREV_BLKP(bp)));
    size_t next_alloc = GET_ALLOC(HDRP(NEXT_BLKP(bp)));
    size_t size = GET_SIZE(HDRP(bp));

    if (prev_alloc && next_alloc) { /* Case 1 */ //noting change
        add_free(bp, size);
        return bp;
    }

    else if (prev_alloc && !next_alloc) { /* Case 2 */
        delete_free(NEXT_BLKP(bp));

        size += GET_SIZE(HDRP(NEXT_BLKP(bp)));

        PUT(HDRP(bp), PACK(size, 0));
        PUT(FTRP(bp), PACK(size,0));
    }

    else if (!prev_alloc && next_alloc) { /* Case 3 */
        delete_free(PREV_BLKP(bp));

        size += GET_SIZE(HDRP(PREV_BLKP(bp)));

        PUT(FTRP(bp), PACK(size, 0));
        PUT(HDRP(PREV_BLKP(bp)), PACK(size, 0));

        bp = PREV_BLKP(bp);
    }

    else { /* Case 4 */
        delete_free(NEXT_BLKP(bp));
        delete_free(PREV_BLKP(bp));

        size += GET_SIZE(HDRP(PREV_BLKP(bp))) + GET_SIZE(FTRP(NEXT_BLKP(bp)));

        PUT(HDRP(PREV_BLKP(bp)), PACK(size, 0));
        PUT(FTRP(NEXT_BLKP(bp)), PACK(size, 0));

        bp = PREV_BLKP(bp);
    }

    add_free(bp, size);
    return bp;
}

static void *find_fit(size_t asize) {
    void *bp;
    int list_i;
    size_t needsize;

    list_i = 0;
    needsize = asize >> 4; //LIST starts 16byte
    //for (list_i = 0; (asize <<= 1) & MINSIZE; list_i++);
    while (list_i < SIZEOFPLIST) {
        if ((free_listp[list_i] != NULL) && (needsize <= 1)) {

            for (bp = free_listp[list_i]; bp != NULL; bp = NEXT_BLOCK(bp)) {

                if (GET_SIZE(HDRP(bp)) >= asize) {
                    return bp;
                }
            }
        }

        needsize >>= 1;
        list_i++;
    }

    return NULL; /* No fit */
}

static void place(void *bp, size_t asize) {
    size_t csize = GET_SIZE(HDRP(bp));
    delete_free(bp);

    if ((csize - asize) >= (MINSIZE)) {
        PUT(HDRP(bp), PACK(asize, 1));
        PUT(FTRP(bp), PACK(asize, 1));
        bp = NEXT_BLKP(bp);
        PUT(HDRP(bp), PACK(csize-asize, 0));
        PUT(FTRP(bp), PACK(csize-asize, 0));
        add_free(bp, csize - asize);
    }
    else {
        PUT(HDRP(bp), PACK(csize, 1));
        PUT(FTRP(bp), PACK(csize, 1));
    }
}

static void add_free(void *bp, size_t size) {
    int list_i;

    size >>= 3; //16byte -> 1, 32 -> 2, ...
    for (list_i = 0; (size >>= 1) != 1; list_i++) {
        if (list_i == SIZEOFPLIST - 1) break;
    }

    if (free_listp[list_i] == NULL) { //first added block
        free_listp[list_i] = bp;
        NEXT_BLOCK(bp) = NULL;
        PRE_BLOCK(bp) = NULL;
    }
    else {
        PRE_BLOCK(free_listp[list_i]) = bp;
        NEXT_BLOCK(bp) = free_listp[list_i];
        free_listp[list_i] = bp;
        PRE_BLOCK(bp) = NULL;
    }
    return;
}

static void delete_free(void *bp) {
    size_t size;
    int list_i;

    size = GET_SIZE(HDRP(bp));
    size >>= 3; //16byte -> 1, 32 -> 2, ...
    for (list_i = 0; (size >>= 1) != 1; list_i++) {
        if (list_i == SIZEOFPLIST - 1) break;
    }

    if (NEXT_BLOCK(bp) == NULL) {
        if (PRE_BLOCK(bp) == NULL) {
            free_listp[list_i] = NULL;
        }
        else {
            NEXT_BLOCK(PRE_BLOCK(bp)) = NULL;
        }
    }
    else {
        if (PRE_BLOCK(bp) == NULL) {
            free_listp[list_i] = NEXT_BLOCK(bp);
            PRE_BLOCK(NEXT_BLOCK(bp)) = NULL;
        }
        else {
            PRE_BLOCK(NEXT_BLOCK(bp)) = PRE_BLOCK(bp);
            NEXT_BLOCK(PRE_BLOCK(bp)) = NEXT_BLOCK(bp);
        }
    }
    return;
}