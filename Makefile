BUILD_AS_LIBRARY 	:= no

UNAME 			:= $(shell uname)

ROOT_DIR    		:= $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
BUILD_DIR   		:= "${ROOT_DIR}/build/"
INSTALL_DIR 		:= "${ROOT_DIR}/install/"

CMAKE_GENERATOR 	:= Unix Makefiles
CMAKE_ARGS_CONFIGURE 	:= -S ${ROOT_DIR} -B ${BUILD_DIR} -G "${CMAKE_GENERATOR}"
CMAKE_ARGS_BUILD 	:= --build ${BUILD_DIR} --parallel
CMAKE_ARGS_INSTALL 	:= --install ${BUILD_DIR} --prefix ${INSTALL_DIR}
ARGS_TEST 		:= --executable ${INSTALL_DIR}/bin/lagrit --levels 1 2

ifeq ($(BUILD_AS_LIBRARY),yes)
  CMAKE_ARGS_CONFIGURE 	+= -D LG_BUILD_AS_LIBRARY:BOOL=ON -D BUILD_SHARED_LIBS=ON
endif

.PHONY: help all configure build test install

all : configure build install

help :
	@echo "LAGRIT BUILD MAKEFILE"
	@echo "--------------------"
	@echo ""
	@echo " targets:"
	@echo ""
	@echo "  all        : configures, builds, installs"
	@echo "  clean      : cleans build directory"
	@echo "  test       : tests executable [exe only]" 
	@echo "  example-c  : runs C interface example [lib only]" 
	@echo "  example-py : runs Python interface example [lib only]" 
	@echo ""
	@echo " options:"
	@echo ""
	@echo "  BUILD_AS_LIBRARY : builds as shared library [yes] or executable [no] (default: $(BUILD_AS_LIBRARY))"
	@echo "  INSTALL_DIR      : directory to store build artifacts (default: $(INSTALL_DIR))"
	@echo "  BUILD_DIR        : directory to build source (default: $(BUILD_DIR))"
	@echo "  CMAKE_GENERATOR  : CMake generator to use (default: $(CMAKE_GENERATOR))"

clean :
	@rm -rf ${BUILD_DIR}

configure :
	@mkdir -p ${BUILD_DIR}
	@cmake ${CMAKE_ARGS_CONFIGURE}

build :
	@cmake ${CMAKE_ARGS_BUILD}

test :
	@cd ${ROOT_DIR}/test/ && python runtests.py

install :
	@cmake ${CMAKE_ARGS_INSTALL}

example-py :
	@if [ "${UNAME}" = "Darwin" ]; then \
		export DYLD_LIBRARY_PATH="${INSTALL_DIR}/lib:${DYLD_LIBRARY_PATH}"; \
	elif [ "${UNAME}" = "Linux" ]; then \
		export LD_LIBRARY_PATH="${INSTALL_DIR}/lib:${LD_LIBRARY_PATH}"; \
	fi; \
	python ${INSTALL_DIR}/examples/example.py

example-c :
	@if [ "${UNAME}" = "Darwin" ]; then \
		export DYLD_LIBRARY_PATH="${INSTALL_DIR}/lib:${DYLD_LIBRARY_PATH}"; \
	elif [ "${UNAME}" = "Linux" ]; then \
		export LD_LIBRARY_PATH="${INSTALL_DIR}/lib:${LD_LIBRARY_PATH}"; \
	fi; \
	${INSTALL_DIR}/examples/lagrit_cxx_example

