.. |nbsp| unicode:: 0xA0
   :trim:

.. |AUnit 3| replace:: AUnit |nbsp| 3
.. |AUnit 2| replace:: AUnit |nbsp| 2

.. _Using_AUnit_with_Restricted_Run-Time_Libraries:

**********************************************
Using AUnit with Restricted Run-Time Libraries
**********************************************

.. index:: Restricted run-time libraries (usage with AUnit)
.. index:: ZFP profile, cert profile
.. index:: VxWorks 653 (and restricted run-time profiles)

|AUnit 3| - like |AUnit 2| - is designed so that it can be used in
environments with restricted Ada run-time libraries, such as ZFP and the cert
run-time profile on Wind River's VxWorks 653.  The patterns given in
this document for writing tests, suites and harnesses are not the only
patterns that can be used with AUnit, but they are compatible with the
restricted run-time libraries provided with GNAT Pro.

.. index:: Dynamic allocation (in test code)

In general, dynamic allocation and deallocation must be used carefully in test
code.  For the cert profile on VxWorks 653, all dynamic allocation must be
done prior to setting the application partition into 'normal' mode.
Deallocation is prohibited in this profile. For some restricted profiles,
dynamic memory management is not provided as part of the run-time,
and should not be used unless you have provided implementations as described
in the GNAT User's Guide Supplement for GNAT Pro Safety-Critical and GNAT
Pro High-Security.

Starting with |AUnit 3|, a simple memory management mechanism has been
included in the framework, using a kind of storage pool. This memory
management mechanism uses a static array allocated at startup, and simulates
dynamic allocation afterwards by allocating parts of this array upon request.
Deallocation is not permitted.

By default, an array of 100KB is allocated. The size can be changed
by modifying the value in the file
:samp:`aunit-{<version>}-src/aunit/framework/staticmemory/aunit-memory.adb`
before building AUnit.

.. index:: AUnit.Memory.Utils.Gen_Alloc

To allocate a new object, you use ``AUnit.Memory.Utils.Gen_Alloc``.

Additional restrictions relevant to the default ZFP profile include:

.. index:: __gnat_last_chance_handler (for ZFP)
.. index:: pragma Weak_External

* Normally the ZFP profile requires a user-defined
  ``__gnat_last_chance_handler`` routine
  to handle raised exceptions. However, AUnit now provides a mechanism to
  simulate exception propagation using gcc builtin :index:`setjmp/longjmp` mechanism.
  This mechanism defines the ``__gnat_last_chance_handler`` routine, so it
  should not be redefined elsewhere. In order to be compatible with this
  restriction, the user-defined last chance handler routine can be defined as a
  "weak" symbol; this way, it will still be linked into the standalone executable,
  but will be replaced by the AUnit implementation when linked with the harness.
  The pragma ``Weak_External`` can be used for that; e.g.:

  .. code-block:: ada

     pragma Weak_External (Last_Chance_Handler);
    
.. index:: GNAT.IO
.. index:: cert profile

* AUnit requires ``GNAT.IO`` provided in :samp:`g-io.ad{?}` in the full or cert
  profile run-time library sources (or as implemented by the user). Since this
  is a run-time library unit it must be compiled with the gnatmake :option:`-a`
  switch.

.. index:: Secondary stack, memcpy, memset

* The AUnit framework has been modified so that no call to the secondary
  stack is performed, nor any call to ``memcpy`` or ``memset``. However, if the unit
  under test, or the tests themselves require use of those routines, then the
  application or test framework must define those symbols and provide the
  requisite implementations.

.. index:: ZFP profile
.. index:: Ada.Calendar

* The timed parameter of the Harness ``Run`` routine has no effect when used
  with the ZFP profile, and on profiles not supporting ``Ada.Calendar``.

