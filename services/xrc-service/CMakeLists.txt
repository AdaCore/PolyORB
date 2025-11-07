cmake_minimum_required(VERSION 3.20)
project(xrc-service VERSION 1.0.0 LANGUAGES CXX)

# C++ Standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Find dependencies
find_package(Threads REQUIRED)

# Main executable
add_executable(xrc-service
    src/main.cpp
)

target_include_directories(xrc-service PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/include
)

target_link_libraries(xrc-service PRIVATE
    Threads::Threads
)

# Compiler flags
target_compile_options(xrc-service PRIVATE
    -Wall -Wextra -Wpedantic
    $<$<CONFIG:Release>:-O3>
)

# Install
install(TARGETS xrc-service DESTINATION bin)
