(use memory-mapped-files files posix lolevel)


(let ((tnpfilpn (create-temporary-file)))
  (let ((tmpfilno (file-open tnpfilpn (+ open/rdwr open/creat)))
        (data "abcde")
        (size 5))
    (file-write tmpfilno data)
    (let ((mmap (map-file-to-memory #f size prot/read (+ map/shared map/file) tmpfilno))
          (str (make-string size)))
      (move-memory! (memory-mapped-file-pointer mmap) str size)
      (assert (blob=? (string->blob data) (string->blob str)))
      (unmap-file-from-memory mmap))))
