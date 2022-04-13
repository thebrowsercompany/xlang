project(swiftcodegen)

if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
    add_compile_options(-Wno-missing-field-initializers)
endif()

add_executable(swiftcodegen "")
target_sources(swiftcodegen PUBLIC
    swift_writer.cpp
    main.cpp
    metadata_cache.cpp
    pch.cpp
    types.cpp)
target_include_directories(swiftcodegen PUBLIC ${XLANG_LIBRARY_PATH} ${PROJECT_BINARY_DIR} ${PROJECT_SOURCE_DIR})
target_compile_definitions(swiftcodegen PUBLIC "SWIFTCODEGENWINRT_VERSION_STRING=\"${XLANG_BUILD_VERSION}\"")

if (WIN32)
    TARGET_CONFIG_MSVC_PCH(swiftcodegen pch.cpp pch.h)
    target_link_libraries(swiftcodegen windowsapp ole32 shlwapi)
else()
    target_link_libraries(swiftcodegen c++ c++swiftcodegen c++experimental)
    target_link_libraries(swiftcodegen -lpthread)
endif()

file(TO_NATIVE_PATH "${CMAKE_CURRENT_BINARY_DIR}/swiftcodegen.exe" swiftcodegenwinrt_exe)
set_target_properties(swiftcodegen PROPERTIES "swiftcodegenwinrt_exe" ${swiftcodegenwinrt_exe})
