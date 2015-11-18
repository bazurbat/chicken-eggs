cmake_minimum_required(VERSION 2.8)

project(crc32 C)

find_package(Chicken REQUIRED)
chicken_find_extension(setup-helper REQUIRED)

chicken_define_extension(crc32
    VERSION 1.0.0
    CATEGORY "crypt"
    DESCRIPTION "Computes CRC checksum"
    LICENSE "BSD"
    AUTHOR "Daishi Kato, rivo")

set(CHICKEN_OPTIONS -optimize-level 3 -debug-level 0 -fixnum-arithmetic -local
    -no-trace -no-procedure-checks -no-bound-checks)

chicken_add_module(crc32 crc32.scm EXTENSION)

chicken_install(EXTENSION crc32 MODULES)
