.. |nbsp| unicode:: 0xA0
   :trim:
   
.. _Overview:

********
Overview
********

How do you write testing code?

The simplest approach is as an expression in a debugger. You can change
debug expressions without recompiling, and you can wait to decide
what to write until you have seen the running objects. You can also
write test expressions as statements that print to the standard
output stream. Both styles of tests are limited because they require
human judgment to analyze their results. Also, they don't compose
nicely - you can only execute one debug expression at a time and a
program with too many print statements causes the dreaded "Scroll
Blindness".

AUnit tests do not require human judgment to interpret, and it is
easy to run many of them at the same time. When you need to test
something, here is what you do:

.. index:: AUnit.Simple_Test_Cases.Test_Case type
.. index:: AUnit.Test_Cases.Test_Case type
.. index:: AUnit.Test_Fixtures.Test_Fixture type

* Derive a test case type from ``AUnit.Simple_Test_Cases.Test_Case``.

  Several test case types are available:

  * ``AUnit.Simple_Test_Cases.Test_Case``: the base type for all test
    cases. Needs overriding of ``Name`` and ``Run_Test``.
  * ``AUnit.Test_Cases.Test_Case``: the traditional AUnit test case type,
    allowing multiple test routines to be registered, where each one is run
    and reported independently.
  * ``AUnit.Test_Fixtures.Test_Fixture``: used together with
    ``AUnit.Test_Caller``, this allows easy creation of test suites comprising
    several test cases that share the same fixture (see :ref:`Fixture`).

  See :ref:`Test_Case` for simple examples of using these types.

* When you want to check a value [#]_ use one of the following Assert [#]_ methods:

  .. [#] While :index:`JUnit` and some other
     members of the xUnit family of unit test frameworks provide specialized forms
     of assertions (e.g. `assertEqual`), we took a design decision in AUnit
     not to provide such forms.  Ada has a much richer type system giving a
     large number of possible scalar types, and leading to an explosion of possible
     special forms of assert routines.  This is exacerbated by the lack of a single
     root type for most types, as is found in Java.  With the introduction of
     AUnit |nbsp| 2 for use with restricted run-time profiles, where even ``'Image`` is
     missing, providing a comprehensive set of special assert routines in the
     framework itself becomes even more unrealistic. Since AUnit is intended to
     be an extensible toolkit, users can certainly write their own custom
     collection of such assert routines to suit local usage.
  
  .. index:: 
     see: Assert subprogram; AUnit.Assertions.Assert
  
  .. [#] Note that in AUnit |nbsp| 3, and contrary to
     AUnit |nbsp| 2, the procedural form of `Assert` has the same behavior whatever
     the underlying Ada run-time library: a failed assertion will cause the
     execution of the calling test routine to be abandoned. The functional form of
     `Assert` always continues on a failed assertion, and provides you
     with a choice of behaviors.

  .. index:: AUnit.Assertions.Assert

  .. code-block:: ada

     AUnit.Assertions.Assert (Boolean_Expression, String_Description);

  or:

  .. code-block:: ada

     if not AUnit.Assertions.Assert (Boolean_Expression, String_Description) then
       return;
     end if;

  .. index:: Assert_Exception subprogram

  If you need to test that a subprogram raises an expected exception, there
  is the procedure ``Assert_Exception`` that takes an access value designating the procedure
  to be tested as a parameter:

  .. code-block:: ada

     type Throwing_Exception_Proc is access procedure;

     procedure Assert_Exception
     (Proc    : Throwing_Exception_Proc;
     Message : String;
     Source  : String := GNAT.Source_Info.File;
     Line    : Natural := GNAT.Source_Info.Line);
     --  Test that Proc throws an exception and record "Message" if not.


  Example:

  .. code-block:: ada

     -- Declared at library level:
     procedure Test_Raising_Exception is
     begin
       call_to_the_tested_method (some_args);
     end Test_Raising_Exception;

     -- In test routine:
     procedure My_Routine (...) is
     begin
       Assert_Exception (Test_Raising_Exception'Access, **String_Description**);
     end My_Routine;

  This procedure can handle exceptions with all run-time profiles (including
  zfp).  If you are using a run-time library capable of propagating exceptions,
  you can use the following idiom instead:

  .. code-block:: ada

     procedure My_Routine (...) is
     begin
       ...
       -- Call subprogram expected to raise an exception:
       Call_To_The_Tested_Method (some_args);
       Assert (False, 'exception not raised');
     exception
       when desired_exception =>
         null;
     end My_Routine;

  An unexpected exception will be recorded as such by the framework.  If you want
  your test routine to continue beyond verifying that an expected exception has
  been raised, you can nest the call and handler in a block.

.. index:: ZFP profile
.. index:: cert profile
.. index:: AUnit.Memory.Utils.Gen_Alloc
.. index:: AUnit.Test_Caller.Create
.. index:: AUnit.Test_Suites.New_Suite

* Create a suite function inside a package to gather together test cases
  and sub-suites. (If either the ZFP or the cert run-time profiles ia being
  used, test cases and suites must be allocated using
  ``AUnit.Memory.Utils.Gen_Alloc``, ``AUnit.Test_Caller.Create``,
  ``AUnit.Test_Suites.New_Suite``, or else they must be statically allocated.)

.. index:: AUnit.Run.Test_Runner
.. index:: AUnit.Run.Test_Runner_With_Status

* At any level at which you wish to run tests, create a harness by
  instantiating procedure ``AUnit.Run.Test_Runner`` or function
  ``AUnit.Run.Test_Runner_With_Status`` with the top-level suite
  function to be executed. This instantiation provides a routine
  that executes all of the tests in the suite. We will call this
  user-instantiated routine `Run` in the text for backward compatibility
  with tests developed for AUnit |nbsp| 1. Note that only one instance of `Run`
  can execute at a time.  This is a tradeoff made to reduce the stack requirement
  of the framework by allocating test result reporting data structures
  statically.


  .. index::
     see: Test_Runner; AUnit.Run.Test_Runner

  .. index:: ZFP profile

  It is possible to pass a filter to a `Test_Runner`, so that only a
  subset of the tests run. In particular, this filter could be initialized from
  a command line parameter. See the package ``AUnit.Test_Filters`` for an
  example of such a filter. AUnit does not automatically initialize this filter
  from the command line both because it would not be supported with some of the
  limited run-time profiles (ZFP for instance), and because you might want to
  pass the argument in different ways (as a parameter to switch, or a stand-alone
  command line argument for instance).

  .. index:: AUnit.Options package

  It is also possible to control the contents of the output report by passing an
  object of type ``AUnit_Options`` to the `Test_Runner`. See package
  ``AUnit.Options`` for details.

* Build the code that calls the harness `Run` routine using
  `gnatmake` or `gprbuild`. The GNAT project file :file:`aunit.gpr` contains all
  necessary switches, and should be imported into your root project file.


