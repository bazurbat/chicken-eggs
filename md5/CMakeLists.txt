cmake_minimum_required(VERSION 2.8)

project(md5 C)

find_package(Chicken REQUIRED)
chicken_find_extension(message-digest 3.0.0 REQUIRED)

chicken_define_extension(md5
    VERSION 3.0.0
    DESCRIPTION "Computes MD5 (RFC1321) checksums"
    CATEGORY "crypt"
    LICENSE "Public Domain"
    AUTHOR "Colin Plumb, packaged for Chicken by Peter Bex")

chicken_add_module(md5 md5.scm EXTENSION)

chicken_install(EXTENSION md5 MODULES)
