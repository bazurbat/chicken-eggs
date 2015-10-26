cmake_minimum_required(VERSION 2.8)

project(concurrent-native-callbacks C)

find_package(Chicken REQUIRED)
chicken_find_extension(typed-records REQUIRED)
chicken_find_extension(matchable REQUIRED)
chicken_find_extension(bind REQUIRED)
chicken_find_extension(miscmacros REQUIRED)
chicken_find_extension(shell REQUIRED)

chicken_define_extension(concurrent-native-callbacks
    VERSION 0.7
    CATEGORY "hell"
    DESCRIPTION "Invoke callbacks from other native threads"
    LICENSE "BSD"
    AUTHOR "bevuta IT GmbH (Felix Winkelmann)"
    URL "git://github.com/chicken-mobile/concurrent-native-callbacks.git")

set(CHICKEN_OPTIONS -optimize-level 3 -debug-level 0 CACHE INTERNAL "")

file(COPY twiddle.c DESTINATION .)

chicken_add_module(concurrent-native-callbacks-compile-time
    cncb-compile-time-module.scm EXTENSION
    DEPENDS cncb-compile-time.scm)
chicken_add_module(concurrent-native-callbacks cncb-module.scm EXTENSION
    DEPENDS cncb.scm twiddle.c concurrent-native-callbacks-compile-time)

chicken_install_modules(concurrent-native-callbacks)
