ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

CMAKE_GENERATOR:=Unix Makefiles
#CMAKE_GENERATOR=Ninja

BUILD_DIR:="${ROOT_DIR}/build/"
INSTALL_DIR="${ROOT_DIR}/install/"

CMAKE_ARGS_CONFIGURE:=-S ${ROOT_DIR} -B ${BUILD_DIR} -G "${CMAKE_GENERATOR}" -D LG_BUILD_AS_LIBRARY:BOOL=ON -D BUILD_SHARED_LIBS=ON
CMAKE_ARGS_BUILD:=--build ${BUILD_DIR} --parallel
CMAKE_ARGS_INSTALL=--install ${BUILD_DIR} --prefix ${INSTALL_DIR}
CMAKE_ARGS_TEST:=--test-dir ${BUILD_DIR} --extra-verbose

.PHONY: all configure build test install

all : configure build install

clean :
	@rm -rf ${BUILD_DIR}

configure :
	@mkdir -p ${BUILD_DIR}
	@cmake ${CMAKE_ARGS_CONFIGURE}

build :
	@cmake ${CMAKE_ARGS_BUILD}

test :
	@ctest ${CMAKE_ARGS_TEST}

install :
	@cmake ${CMAKE_ARGS_INSTALL}

example1 :
	@DYLD_LIBRARY_PATH="${INSTALL_DIR}/lib:${DYLD_LIBRARY_PATH}" ${INSTALL_DIR}/examples/lagrit_cxx_example

example2 :
	@DYLD_LIBRARY_PATH="${INSTALL_DIR}/lib:${DYLD_LIBRARY_PATH}" python ${INSTALL_DIR}/examples/example.py
