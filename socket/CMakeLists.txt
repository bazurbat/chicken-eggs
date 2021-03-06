cmake_minimum_required(VERSION 2.8)

project(socket C)

find_package(Chicken REQUIRED)
chicken_find_extension(foreigners)

chicken_define_extension(${PROJECT_NAME}
    VERSION 0.2.6
    DESCRIPTION "Interface to the BSD socket API"
    CATEGORY "net"
    LICENSE "BSD"
    AUTHOR "Jim Ursetto")

include(CheckSymbolExists)

set(features AF_UNIX SO_USELOOPBACK SO_REUSEPORT SO_TIMESTAMP
    SO_EXCLUSIVEADDRUSE TCP_MAXSEG TCP_NOPUSH TCP_NOOPT TCP_KEEPALIVE IP_MTU
    IP_MTU_DISCOVER IP_PKTINFO IP_RECVERR IP_RECVTOS IP_RECVTTL
    IP_ROUTER_ALERT IP_RECVOPTS IP_RECVRETOPTS IP_RETOPTS IP_RECVDSTADDR
    IPV6_V6ONLY IPV6_ADDRFORM IPV6_MTU IPV6_MTU_DISCOVER IPV6_MULTICAST_HOPS
    IPV6_MULTICAST_IF IPV6_MULTICAST_LOOP IPV6_PKTINFO IPV6_RTHDR IPV6_AUTHHDR
    IPV6_DSTOPTS IPV6_HOPOPTS IPV6_FLOWINFO IPV6_HOPLIMIT IPV6_RECVERR
    IPV6_ROUTER_ALERT IPV6_UNICAST_HOPS IPV6_NEXTHOP IPV6_PORT_RANGE
    IPV6_JOIN_GROUP IPV6_LEAVE_GROUP IPV6_CHECKSUM IPPROTO_IPV6)

file(WRITE socket-config.scm "; GENERATED\n")
foreach(feature ${features})
    check_symbol_exists(${feature} "${PROJECT_SOURCE_DIR}/socket.h" HAVE_${feature})
    if(HAVE_${feature})
        file(APPEND socket-config.scm "(register-feature! '${feature})\n")
    else()
        file(APPEND socket-config.scm "(unregister-feature! '${feature})\n")
    endif()
endforeach()

set(CHICKEN_OPTIONS -optimize-level 2 -debug-level 1
    -extend feature-test-syntax
    -extend ${PROJECT_SOURCE_DIR}/socket-config.scm
    -D scan-buffer-line-returns-3-vals)

chicken_add_module(socket socket-mod.scm EXTENSION)

chicken_install(EXTENSION socket MODULES)
