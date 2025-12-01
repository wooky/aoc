find_program(go
  NAMES go
  REQUIRED
)

file(GLOB_RECURSE GO_SOURCES *.go)
set(GO_LIB "${CMAKE_BINARY_DIR}/libaoc_go.so")

add_custom_command(
  OUTPUT ${GO_LIB}
  COMMAND ${go} build -o "${GO_LIB}" -buildmode=c-shared
  DEPENDS ${GO_SOURCES}
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)
add_custom_target(aoc_go ALL
  DEPENDS ${GO_LIB}
)
