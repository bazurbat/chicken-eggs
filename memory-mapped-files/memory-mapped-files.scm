;;;; memory-mapped file access for UNIX and Windows


#>
#ifdef WIN32

#define PROT_NONE       0
#define PROT_READ       1
#define PROT_WRITE      2
#define PROT_EXEC       4
#define MAP_FILE        0
#define MAP_SHARED      1
#define MAP_PRIVATE     2
#define MAP_FIXED       0x10
#define MAP_ANONYMOUS   0x20

// This value is available starting with Windows XP with SP2 
// and Windows Server 2003 with SP1.
#ifndef FILE_MAP_EXECUTE
#define FILE_MAP_EXECUTE 0x20
#endif//FILE_MAP_EXECUTE

static int page_flags[] =
{
    0,
    PAGE_READONLY,
    PAGE_READWRITE,
    PAGE_READWRITE,
    PAGE_EXECUTE_READ,
    PAGE_EXECUTE_READ,
    PAGE_EXECUTE_READWRITE
};

static int file_flags[] =
{
    0,
    FILE_MAP_READ,
    FILE_MAP_READ|FILE_MAP_WRITE,
    FILE_MAP_READ|FILE_MAP_WRITE,
    FILE_MAP_READ|FILE_MAP_EXECUTE,
    FILE_MAP_READ|FILE_MAP_EXECUTE,
    FILE_MAP_READ|FILE_MAP_WRITE|FILE_MAP_EXECUTE
};

void* mmap(void* addr,int len,int prot,int flags,int fd,int off)
{
    HANDLE hMap;
    HANDLE hFile;

    void* ptr;

    if ((flags & MAP_FIXED) || (flags & MAP_PRIVATE) || (flags & MAP_ANONYMOUS))
    {
        errno = EINVAL;
        return (void*)-1;
    }

    /*
     * We must cast because _get_osfhandle returns intptr_t, but it must
     * be compared with INVALID_HANDLE_VALUE, which is a HANDLE type.
     * Who comes up with this shit?
     */
    hFile = (HANDLE)_get_osfhandle(fd);
    if (hFile == INVALID_HANDLE_VALUE)
    {
        return (void*)-1;
    }

    hMap = CreateFileMapping(
            hFile,
            NULL,
            page_flags[prot & (PROT_READ|PROT_WRITE|PROT_EXEC)],
            0,
            0,
            NULL);

    if (hMap == INVALID_HANDLE_VALUE)
    {
        set_last_errno();
        return (void*)-1;
    }

    ptr = MapViewOfFile(
            hMap,
            file_flags[prot & (PROT_READ|PROT_WRITE|PROT_EXEC)],
            0,
            off,
            len);

    if (ptr == NULL)
    {
        set_last_errno();
        ptr = (void*)-1;
    }

    CloseHandle(hMap);

    return ptr;
}

int munmap(void* addr,int len)
{
    if (UnmapViewOfFile(addr))
    {
        errno = 0;
        return 0;
    }
    set_last_errno();
    return -1;
}
#else
#include <sys/mman.h>
#endif

int is_bad_mmap(void* p)
{
    void* bad_ptr;
    bad_ptr = (void*)-1;
    return p == bad_ptr;
}

<#


(module memory-mapped-files (prot/none
			     prot/read
			     prot/write
			     prot/exec
			     map/file 
			     map/shared
			     map/private
			     map/fixed
			     map/anonymous
			     map-file-to-memory
			     unmap-file-from-memory
			     memory-mapped-file-pointer
			     memory-mapped-file?)

  (import scheme chicken foreign)

(define posix-error
  (let ([strerror (foreign-lambda c-string "strerror" int)]
	[string-append string-append] )
    (lambda (type loc msg . args)
      (let ([rn (##sys#update-errno)])
	(apply ##sys#signal-hook type loc (string-append msg " - " (strerror rn)) args) ) ) ) )

;;; Memory mapped I/O:

(define-foreign-variable _prot_read int "PROT_READ")
(define-foreign-variable _prot_write int "PROT_WRITE")
(define-foreign-variable _prot_exec int "PROT_EXEC")
(define-foreign-variable _prot_none int "PROT_NONE")

(define prot/read _prot_read)
(define prot/write _prot_write)
(define prot/exec _prot_exec)
(define prot/none _prot_none)

(define-foreign-variable _map_fixed int "MAP_FIXED")
(define-foreign-variable _map_shared int "MAP_SHARED")
(define-foreign-variable _map_private int "MAP_PRIVATE")
(define-foreign-variable _map_anonymous int "MAP_ANON")
(define-foreign-variable _map_file int "MAP_FILE")

(define map/fixed _map_fixed)
(define map/shared _map_shared)
(define map/private _map_private)
(define map/anonymous _map_anonymous)
(define map/file _map_file)

(define map-file-to-memory
  (let ((mmap (foreign-lambda c-pointer "mmap" c-pointer integer int int int integer))
	(bad-mmap? (foreign-lambda bool "is_bad_mmap" c-pointer)))
    (lambda (addr len prot flag fd . off)
      (let ((addr (if (not addr) (##sys#null-pointer) addr))
            (off (if (pair? off) (car off) 0)) )
        (unless (and (##core#inline "C_blockp" addr) (##core#inline "C_specialp" addr))
	  (##sys#signal-hook #:type-error 'map-file-to-memory "bad argument type - not a foreign pointer" addr) )
        (let ((addr2 (mmap addr len prot flag fd off)))
          (when (bad-mmap? addr2)
	    (posix-error #:file-error 'map-file-to-memory "cannot map file to memory" addr len prot flag fd off) )
          (##sys#make-structure 'mmap addr2 len) ) ) ) ) )

(define unmap-file-from-memory
  (let ((munmap (foreign-lambda int "munmap" c-pointer integer)) )
    (lambda (mmap . len)
      (##sys#check-structure mmap 'mmap 'unmap-file-from-memory)
      (let ((len (if (pair? len) (car len) (##sys#slot mmap 2))))
        (unless (eq? 0 (munmap (##sys#slot mmap 1) len))
	  (posix-error #:file-error 'unmap-file-from-memory "cannot unmap file from memory" mmap len) ) ) ) ) )

(define (memory-mapped-file-pointer mmap)
  (##sys#check-structure mmap 'mmap 'memory-mapped-file-pointer)
  (##sys#slot mmap 1) )

(define (memory-mapped-file? x)
  (##sys#structure? x 'mmap) )

)
