/* lowlevel hackery - accessed via "bind" */


static ___symbol
extract_callback_name(void *buf)
{
  return *((char **)buf);
}


static void
send_termination_message(int fd)
{
  void *n = NULL;
  
  write(fd, &n, sizeof(void *));
}


static void *
extract_argument_ptr(___scheme_value str)
{
  return *((void **)C_data_pointer(str));
}


static void
dump_data(void **ptr)
{
  int i;

  for(i = 0; i < 4; ++i) {
    printf("%p: %p\n", ptr, *ptr);
    ptr += 2;
  }
}
