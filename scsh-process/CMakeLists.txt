cmake_minimum_required(VERSION 2.8)

project(scsh-process C)

find_package(Chicken REQUIRED)

chicken_define_extension(scsh-process
    VERSION 0.4.1
    CATEGORY "os"
    LICENSE "BSD"
    AUTHOR "Peter Bex"
    URL "https://bitbucket.org/sjamaan/scsh-process")

# TODO: investigate how to implement feature check based on Chicken version

set(CHICKEN_OPTIONS -optimize-level 3 -debug-level 0)

chicken_add_module(scsh-process scsh-process.scm EXTENSION)

chicken_install_modules(scsh-process)
