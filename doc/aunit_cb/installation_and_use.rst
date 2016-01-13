.. _Installation_and_Use:

********************
Installation and Use
********************

.. index:: Installation of AUnit, ZFP profile, cert profile

AUnit 3 contains support for restricted runtimes such as the zero-foot-print
(ZFP) and certified (cert) profiles. It can now be installed simultaneously
for several targets and runtimes.

.. _Note_on_gprbuild:

Note on gprbuild
================

.. index:: gprbuild, gprinstall

In order to compile, install and use AUnit, you need `gprbuild` and `gprinstall`
version 2.2.0 or above.

.. _Support_for_other_platforms/run-times:

Support for other platforms/run-times
=====================================

AUnit should be built and installed separately for each target and runtime
it is meant to be used with. The necessary customizations are performed at
AUnit build time, so once the framework is installed, it is always referenced
simply by adding the line

::

  with "aunit";
  
to your project.

.. _Installing_AUnit:

Installing AUnit
================

Normally AUnit comes preinstalled and ready-to-use for all runtimes in
your GNAT distribution. The following instructions are for rebuilding it from
sources for a custom configuration that the user may have. 

* Extract the archive:

  ::

       $ gunzip -dc aunit-<version>-src.tgz | tar xf -
    

* To build AUnit for a full Ada run-time:

  ::

       $ cd aunit-<version>-src
       $ make
    
.. index:: ZFP profile

* To build AUnit for a ZFP run-time targeting powerpc-elf platform:

  ::

       $ cd aunit-<version>-src
       $ make TARGET=powerpc-elf RTS=zfp
    

* To build AUnit for a reconfigurable runtime zfp-leon3 targeting leon3-elf
  platform:

  ::

       $ cd aunit-<version>-src
       $ make TARGET=leon3-elf RTS=zfp RTS_CONF="--RTS=zfp-leon3"
    

Once the above build procedure has been performed for the desired platform, you
can install AUnit:

   ::

       $ make install INSTALL=<install-root>

.. index:: gprbuild

We recommend that you install AUnit into the standard location used by `gprbuild`
to find the libraries for a given configuration. For example for the case above
(runtime `zfp-leon3` targeting `leon3-elf`), the default location is
:samp:`{<gnat-root>}/leon3-elf/zfp-leon3`. If the runtime is located in a custom
directory and specified by the full path, using this exact path also as
*<install_root>* is a sensible choice.

If ``INSTALL`` is not specified, then AUnit will use the root directory where
`gprbuild` is installed.

* Specific installation:

  The AUnit makefile supports some specific options, activated using
  environment variables. The following options are defined:

  .. index:: INSTALL environment variable

  * ``INSTALL``: defines the AUnit base installation directory, set to
    gprbuild's base installation directory as found in the ``PATH``.

  .. index:: TARGET environment variable

  * ``TARGET``: defines the gnat tools prefix to use. For example, to compile
    AUnit for powerpc VxWorks, ``TARGET`` should be set to ``powerpc-wrs-vxworks``. If not
    set, the native compiler will be used.

  .. index:: RTS environment variable

  * ``RTS``:  defines both the run-time used to compile AUnit and the value
    given to the AUnit project as ``RUNTIME`` scenario variable.

  .. index:: RTS_CONF environment variable

  * ``RTS_CONF``: defines the `gprbuild` Runtime config flag. The value is
    set to ``--RTS=$(RTS)`` by default. Can be used when compiling AUnit for a
    configurable run-time.

* To test AUnit:

  The AUnit test suite is in the test subdirectory of the source package.

  ::

       $ cd test
       $ make
    
  The test suite's makefile supports the following variables:

  * ``RTS``
  * ``TARGET``

.. _Installed_files:

Installed files
===============

The AUnit library is installed in the specified directory (*<aunit-root>*
identifies the root installation directory as specified during the installation
procedures above):

.. index:: aunit.gpr project file

* the :file:`aunit.gpr` project is installed in :samp:`{<aunit-root>}/lib/gnat`
* the AUnit source files are installed in :samp:`{<aunit-root>}/include/aunit`
* the AUnit library files are installed in :samp:`{<aunit-root>}/lib/aunit`
* the AUnit documentation is installed in :samp:`{<aunit-root>}/share/doc/aunit`
* the AUnit examples are installed in :samp:`{<aunit-root>}/share/examples/aunit`

