project(aoc_go)

find_program(go
  NAMES go
  REQUIRED
)

add_custom_target(${PROJECT_NAME} ALL
  COMMAND ${go} build -o "${CMAKE_BINARY_DIR}/lib${PROJECT_NAME}.so" -buildmode=c-shared
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)
