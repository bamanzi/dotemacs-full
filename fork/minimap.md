minimap.el --- Minimap sidebar for Emacs

Copyright (C) 2012 Wei Liu
Copyright (C) 2011 Dustin Lacewell

Authors: Wei Liu <liuw@liuw.name> 
         Dustin Lacewell <dlacewell@gmail.com>
         David Engster <dengste@eml.cc>
Keywords: minimap
Version: 0.1

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

==Commentary:==

Dustin Lacewell:
Heavily based off minimap.el by David Engster however I
have refactored it a bit to support only a single global
minimap window. Usually this should be opened on startup
when there is a single window. Some defaults have also
been changed.

Wei Liu: 
This work is based on Dustin Lacewell and David Engster's work of
minimap. In this version, a global minimap sidebar is created and
persistent through Emacs. Other modifications include bug fix and only
enable minimap for graphical mode Emacs session.

This file is an implementation of a minimap sidebar, i.e., a
smaller display of the current buffer on the left side. It
highlights the currently shown region and updates its position
automatically. You can navigate in the minibar by dragging the
active region with the mouse, which will scroll the corresponding
edit buffer.

Usage:
 * Put minimap.el in your load path.
 * (require 'minimap)
 * Use 'M-x minimap-toggle' to toggle the minimap.
 * Use 'M-x minimap-create' to create the minimap.
 * Use 'M-x minimap-kill' to kill the minimap.
 * Use 'M-x customize-group RET minimap RET' to adapt minimap to your needs.
