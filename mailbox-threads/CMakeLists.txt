cmake_minimum_required(VERSION 2.8)

project(mailbox-threads C)

find_package(Chicken REQUIRED)
chicken_find_extension(mailbox REQUIRED)

chicken_define_extension(mailbox-threads
    VERSION 1.4
    DESCRIPTION "An enhancement for srfi-18 that connects a mailbox to each thread."
    CATEGORY "hell"
    LICENSE "BSD"
    AUTHOR "Christian Kellermann")

set(CHICKEN_OPTIONS -optimize-level 2 -debug-level 0)

chicken_add_module(mailbox-threads mailbox-threads.scm EXTENSION)

chicken_install_modules(mailbox-threads)
