cmake_minimum_required(VERSION 2.8)

project(fmt C)

find_package(Chicken REQUIRED)
chicken_find_extension(utf8 REQUIRED)

chicken_define_extension(${PROJECT_NAME}
    VERSION 1.4.1
    DESCRIPTION "Combinator Formatting"
    CATEGORY "io"
    LICENSE "BSD"
    AUTHOR "Alex Shinn")

set(CHICKEN_OPTIONS -optimize-level 2 -debug-level 0)

chicken_add_module(fmt fmt-chicken.scm EXTENSION)

chicken_add_module(fmt-c fmt-c-chicken.scm EXTENSION
    DEPENDS fmt-c.scm fmt)

chicken_add_module(fmt-js fmt-js-chicken.scm EXTENSION
    DEPENDS fmt-js.scm fmt fmt-c)

chicken_add_module(fmt-color fmt-color-chicken.scm EXTENSION
    DEPENDS fmt-color.scm fmt)

chicken_add_module(fmt-unicode fmt-unicode-chicken.scm EXTENSION
    DEPENDS fmt)

# TODO: add install documentation

chicken_install_modules(${PROJECT_NAME})
