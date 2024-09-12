cmake_minimum_required(VERSION 3.5 FATAL_ERROR)

set (CMAKE_CXX_STANDARD 23)

project(runtime LANGUAGES CXX)

file(GLOB_RECURSE SRCS src/*)

add_library(runtime STATIC ${SRCS})