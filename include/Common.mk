# CDDL HEADER START
# -----------------------------------------------------------------------
# The contents of this file are subject to the Common Development and 
# Distribution License, Version 1.0 (the "License"); you may not use 
# this file except in compliance with the License.  You should have 
# received a copy of the Common Development and Distribution License 
# along with this software.  If not, it can be retrieved online at 
# http://www.opensource.org/licenses/CDDL-1.0
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
# the License for the specific language governing rights and limitations
# under the License.
# 
# When distributing Covered Code, include this CDDL Header Notice in
# each file and include the License file at CDDL-LICENSE.  If applicable
# add the following below the CDDL Header, with the fields enclosed
# by brackets replaced by your own identifying information.
# "Portions Copyright [year] [name of copyright owner]"
# 
# Copyright 2012, 2013 Beads D. Land-Trujillo.  All Rights Reserved.
# -----------------------------------------------------------------------
# CDDL HEADER END

include include/Header.mk

#
# All good rules
#

.PHONY:	all good todo docs compile current neat clean push make

all:		push compile good

good:		$(POSEBIN)/pose.beam
	@$(ERL) $(SUPERL) $(POSURE) $(STOP)
	
$(POSEBIN)/pose.beam:
	@if [ ! -f $(POSEBIN)/pose.beam ]
	$(error Must compile pose to do good)

#
# Rules to regenerate documentation
#

docs:	README.md \
			$(patsubst src/%.erl, doc/%.md, $(wildcard src/*.erl))

doc/%.md:	src/%.erl
	@$(CROWBAR:_cmds_=doc)

#
# Temporary todo rules pending proper 2do_go4 implementation
#

todo:	README.md
	@git add -f $(TODO_FILES)
	@if ! git diff-index --cached --quiet HEAD; \
		then (git commit $(TODO_FILES) -m "updated todo"); fi

README.md:	doc/TODO_head.edoc doc/overview.edoc src/overview.hrl
	@$(CROWBAR:_cmds_=doc)

doc/TODO_head.edoc:		TODO.edoc
	@if [ $(TODO_MORE) -gt 0 ]; \
		then (head -7 TODO.edoc; \
			  echo "@todo ...plus $(TODO_MORE) more (see TODO.edoc)"); fi \
		> doc/TODO_head.edoc

TODO.edoc:	;

#
# Rules for compiling 
#

compile:	neat
	@$(CROWBAR:_cmds_=compile doc)

current:	neat make
	@if [ "$(ONLINE)" == yes ]; \
		then $(CROWBAR:_cmds_=update-deps compile doc); \
		else $(CROWBAR:_cmds_=compile doc); fi

clean:		neat make
	@if [ "$(ONLINE)" == yes ]; \
		then (rm -rf deps; $(CROWBAR:_cmds_=clean get-deps)); \
		else ($(CROWBAR:_cmds_=clean)); fi

neat:
	@rm -f *.dump

#
# Rules for managing revisions and synchronized common files
#

push:	make
	@if [ "$(DEV)" == yes -a "$(ONLINE)" == yes ]; \
		then (git push origin master); fi

make:	$(patsubst include/%.mk, \
			include/$(B_PREFIX)%.mk$(B_SUFFIX), \
			$(wildcard include/*.mk))
	@if [ "$(shell basename $(CURDIR))" != nosh ]; \
		then ($(UNISON) -merge "$(MERGE)"); fi

include/$(B_PREFIX)%.mk$(B_SUFFIX):		include/%.mk
	@if [ ! -f $@ ]; \
		then ($(UNISON) && (test -f $@ || cp $< $@)); fi