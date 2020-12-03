# -*- coding: utf-8 -*-
#
# GNAT build configuration file

import sys
import os
import time
import re

sys.path.append('.')

import ada_pygments
import latex_elements

# Some configuration values for the various documentation handled by
# this conf.py

DOCS = {
    'aunit_cb': {
        'title': u'AUnit Cookbook'}}

# Then retrieve the source directory
root_source_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
texi_fsf = True  # Set to False when FSF doc is switched to sphinx by default


def get_copyright():
    return u'2008-%s, AdaCore' % time.strftime('%Y')


def get_version():
    # Assumes that version number is defined in file version_information
    # two directory levels up, as the first line in this file
    try:
        with open('../../version_information') as vinfo:
            line = (vinfo.readline()).strip()
            return line
    except:
        print('Error opening or reading version_information file')
        sys.exit(1)

# First retrieve the name of the documentation we are building
doc_name = os.environ.get('DOC_NAME', None)
if doc_name is None:
    print('DOC_NAME environment variable should be set')
    sys.exit(1)

if doc_name not in DOCS:
    print('%s is not a valid documentation name' % doc_name)
    sys.exit(1)

# Exclude sources that are not part of the current documentation
exclude_patterns = []
for d in os.listdir(root_source_dir):
    if d not in ('share', doc_name, doc_name + '.rst'):
        exclude_patterns.append(d)
        print('ignoring %s' % d)

extensions = []
templates_path = ['_templates']
source_suffix = '.rst'
master_doc = doc_name

# General information about the project.
project = DOCS[doc_name]['title']

copyright = get_copyright()

version = get_version()
release = get_version()

pygments_style = None
html_theme = 'sphinxdoc'
if os.path.isfile('adacore_transparent.png'):
    html_logo = 'adacore_transparent.png'
if os.path.isfile('favicon.ico'):
    html_favicon = 'favicon.ico'

html_static_path = ['_static']

latex_elements = {
    'preamble': latex_elements.TOC_DEPTH +
    latex_elements.PAGE_BLANK +
    latex_elements.TOC_CMD +
    latex_elements.LATEX_HYPHEN +
    latex_elements.doc_settings(DOCS[doc_name]['title'],
                                get_version()),
    'tableofcontents': latex_elements.TOC}

latex_documents = [
    (master_doc, '%s.tex' % doc_name, project, u'AdaCore', 'manual')]

texinfo_documents = [
    (master_doc, doc_name, project,
     u'AdaCore', doc_name, doc_name, '')]


def setup(app):
    app.add_lexer('ada', ada_pygments.AdaLexer())
    app.add_lexer('gpr', ada_pygments.GNATProjectLexer())
