# Copyright (C) 2007-2012 LuaDist.
# Created by Peter Kapec
# Redistribution and use of this file is allowed according to the terms of the MIT license.
# For details see the COPYRIGHT file distributed with LuaDist.
# Please note that the package source code is licensed under its own license.

project ( epeg NONE )
cmake_minimum_required ( VERSION 2.8 )
include ( cmake/dist.cmake )

# Install all files and documentation
install ( FILES epeg.lua DESTINATION ${INSTALL_LMOD} )
install ( FILES epegscanner.lua epeg.epeg DESTINATION ${INSTALL_FOO} )
install ( FILES COPYING README TODO DESTINATION ${INSTALL_DATA} )
