.. _Test_Case:

*********
Test Case
*********

In this chapter, we will introduce how to use the various forms of Test
Cases. We will illustrate with a very simple test routine, which verifies
that the sum of two Money values with the same currency unit is a value
that is the sum of the two values:

.. code-block:: ada

   declare
     X, Y: Some_Currency;
   begin
     X := 12; Y := 14;
     Assert (X + Y = 26, "Addition is incorrect");
   end;

The following sections will show how to use this test method using the
different test case types available in AUnit.


.. index:: AUnit.Options

AUnit.Options
=============

``Options`` is a small package containing only a record ``AUnit.Options``.

The record definition is at follow: 

.. code-block:: ada

   type AUnit_Options is record
      Global_Timer     : Boolean := False;
      Test_Case_Timer  : Boolean := False;
      Report_Successes : Boolean := True;
      Capture_Standard : Boolean := False;
      Filter           : AUnit.Test_Filters.Test_Filter_Access := null;
   end record;

- ``Global_Timer`` : If ``True``, mesure the time taken to run all the test of the session.
- ``Test_Case_Timer`` : If ``True``, mesure the individual time taken to each test of the session.
- ``Report_Successes`` : If ``True``, report the successful test. Only used for non-XML reporters.
- ``Capture_Standard`` : If ``True``, redirect stdout and stderr to include them in the report. Only available for 
the ``Junit`` reporter.
- ``Filter`` : Used to filter which test to report, see the package ``AUnit.Test_Filters`` for an 
  example of such a filter.


.. index:: AUnit.Test_Info

AUnit.Test_Info
===============

``Test_Info`` is a package defining two records : ``Tested_Location`` and ``Test_Suffix``.

``Tested_Location`` is a record describing the location of a the function tested by a specific test
and is defined as follows : 

.. code-block:: ada

   type Tested_Location is record
      Tested_File   : Message_String;
      Tested_Line   : Natural;
      Tested_Column : Natural;
      Tested_Name : Message_String;
   end record;
   type Tested_Location_Access is access all Tested_Location;

- ``Tested_File``: The name of the file the tested function is located in.
- ``Tested_Line``: The line number of the file the tested function is located in.
- ``Tested_Column``: The column number of the file the tested function is located in.
- ``Tested_Name``: The name of the tested function.

``Test_Suffix`` is a record handling the case where the tested function deepened on another 
(instance/inheritance/overridden/...). It is defined as follows : 

.. code-block:: ada 

   type Test_Suffix_Access is access Test_Suffix;
   type Test_Suffix is record
      Suffix_Text       : Message_String;
      Suffix_Location   : Tested_Location_Access;
      Additional_Suffix : Test_Suffix_Access;
   end record;

- ``Suffix_Text`` : The type of relation between the tested function and the function it dependes on. (can be `instance at`, `inherited at`, `overriden at`,...).
- ``Suffix_Location`` : The location of the other function.
- ``Additional_Suffix`` : In case the other function also depending on another function.

.. index:: AUnit.Simple_Test_Cases.Test_Case type

.. _AUnit-Simple_Test_Cases:

AUnit.Simple_Test_Cases
=======================

``AUnit.Simple_Test_Cases.Test_Case`` is the root type of all test
cases. Although generally not meant to be used directly, it provides a
simple and quick way to run a test.

This tagged type has several methods that need to be defined, or may be
overridden.

.. index:: Name abstract function

* ``function Name (T : Test_Case) return Message_String is abstract``:

  This function returns the Test name. You can easily translate regular
  strings to ``Message_String`` using ``AUnit.Format``. For example:

  .. code-block:: ada

     function Name (T : Money_Test) return Message_String is
     begin
       return Format ("Money Tests");
     end Name;

.. index:: Package_Name abstract function

* ``function Package_Name (T : Test_Case) return Message_String is abstract``:

  This function returns the package name of the Test. For example:

  .. code-block:: ada

     function Package_Name (T : Money_Test) return Message_String is
     begin
       return Format ("Money_Tests.Suite");
     end Name;

.. index:: Location abstract function

* ``function Location (T : Test_Case) return Tested_Location is abstract``:

  This function returns information about the location of the Test. For example:

  .. code-block:: ada

     function Location (T : Money_Test) return Tested_Location is
     begin
       return
        (Tested_File   => Format ("money_tests-suite.adb"),
         Tested_Line   => 16,
         Tested_Column => 4,
         Tested_Name => null);
     end Name;


.. index:: Suffix abstract function

* ``function Suffix (T : Test_Case) return Test_Suffix is abstract``:

  This function returns additional information about the location of the Test. For example:

  .. code-block:: ada

     function Suffix (T : Money_Test) return Test_Suffix_Access is
     begin
       return new Test_Suffix'(
         Suffix_Text => "instance at"
         Suffix_Location => new Tested_Location'(
            Tested_File => Format ("money_test-suite.adb"),
            Tested_Line => 4, 
            Tested_Column => 4, 
            Tested_Name => null
         ),
         Additional_Suffix => null
       );
     end Name;


.. index:: Run_Test abstract function

* ``procedure Run_Test (T : in out Test_Case) is abstract``:

  This procedure contains the test code. For example:
  
  .. code-block:: ada

     procedure Run_Test (T : in out Money_Test) is
       X, Y: Some_Currency;
     begin
       X := 12; Y := 14;
       Assert (X + Y = 26, "Addition is incorrect");
     end Run_Test;

.. index:: Set_Up procedure, Tear_Down procedure

* ``procedure Set_Up (T : in out Test_Case);`` and ``procedure Tear_Down (T : in out Test_Case);``
  (default implementations do nothing):

  These procedures are meant to respectively set up or tear down the
  environment before running the test case. See :ref:`Fixture` for examples
  of how to use these methods.

You can find a compilable example of
``AUnit.Simple_Test_Cases.Test_Case`` usage in your AUnit installation
directory: :samp:`{<aunit-root>}/share/examples/aunit/simple_test/` or from the
source distribution :samp:`aunit-{<version>}-src/examples/simple_test/`.

.. _AUnit-Test_Cases:

AUnit.Test_Cases
================

``AUnit.Test_Cases.Test_Case`` is derived from
``AUnit.Simple_Test_Cases.Test_Case`` and defines its
``Run_Test`` procedure.

It allows a very flexible composition of Test routines inside a single
test case, each being reported independently.

The following subprograms must be considered for inheritance, overriding, or
completion:

.. index:: Name abstract function (for AUnit.Test_Cases.Test_Case)
.. index:: Package_Name abstract function (for AUnit.Test_Cases.Test_Case)
.. index:: Location abstract function (for AUnit.Test_Cases.Test_Case)
.. index:: Suffix abstract function (for AUnit.Test_Cases.Test_Case)

* | ``function Name (T : Test_Case) return Message_String is abstract;``
  | ``function Package Name (T : Test_Case) return Message_String is abstract;``
  | ``function Location (T : Test_Case) return Tested_Location is abstract;``
  | ``function Suffix (T : Test_Case) return Test_Suffix_Access is abstract;``
  Inherited. See :ref:`AUnit.Simple_Test_Cases<AUnit-Simple_Test_Cases>`.

.. index:: Set_Up procedure (for AUnit.Test_Cases.Test_Case)
.. index:: Tear_Down procedure (for AUnit.Test_Cases.Test_Case)


* | ``procedure Set_Up (T : in out Test_Case);`` 
  | ``procedure Tear_Down (T : in out Test_Case);``

  Inherited. See :ref:`AUnit.Simple_Test_Cases<AUnit-Simple_Test_Cases>`.

.. index:: Set_Up_Case procedure (for AUnit.Test_Cases.Test_Case)
.. index:: Tear_Down_Case procedure (for AUnit.Test_Cases.Test_Case)


* | ``procedure Set_Up_Case (T : in out Test_Case);``
  | ``procedure Tear_Down_Case (T : in out Test_Case);``

  Default implementation does nothing.

  These last two procedures provide an opportunity to set up and tear down the test
  case before and after all test routines have been executed. In contrast, the
  inherited ``Set_Up`` and ``Tear_Down`` are called before and after the
  execution of each individual test routine.

.. index:: Register abstract procedure (for AUnit.Test_Cases.Test_Case)

* ``procedure Register_Tests (T : in out Test_Case) is abstract;``

  .. index:: Registration.Register_Routine
  .. index:: Specific_Test_Case.Register_Wrapper

  This procedure must be overridden. It is responsible for registering all
  the test routines that will be run. You need to use either
  ``Registration.Register_Routine`` or the generic
  ``Specific_Test_Case.Register_Wrapper`` subprograms defined in
  ``AUnit.Test_Cases`` to register a routine. A test routine has the
  form:

  .. code-block:: ada

     procedure Test_Routine (T : in out Test_Case'Class);

  or

  .. code-block:: ada
  
     procedure Test_Wrapper (T : in out Specific_Test_Case'Class);

  The former procedure is used mainly for dispatching calls
  (see :ref:`OOP_considerations`).


Using this type to test our money addition, the package spec is:

.. code-block:: ada

   with AUnit; use AUnit;
   with AUnit.Test_Cases; use AUnit.Test_Cases;

   package Money_Tests is

     type Money_Test is new Test_Cases.Test_Case with null record;

     procedure Register_Tests (T: in out Money_Test);
     -- Register routines to be run

     function Name (T: Money_Test) return Message_String;
     -- Provide name identifying the test case

     function Location (T: Money_Test) return Tested_Location;  
     -- Provide the location of the tested function.

     function Suffix (T: Money_Test) return Test_Suffix_Access;
     -- Provide additional information about the location of the tested
     -- function

     function Package_Name (T: Money_Test) return Message_String;
     -- Provide the name of the package the test is located in.

     -- Test Routines:
     procedure Test_Simple_Add (T : in out Test_Cases.Test_Case'Class);
   end Money_Tests

The package body is:

.. code-block:: ada

   with AUnit.Assertions; use AUnit.Assertions;

   package body Money_Tests is

      procedure Test_Simple_Add (T : in out Test_Cases.Test_Case'Class) is
         X, Y : Some_Currency;
      begin
         X := 12; Y := 14;
         Assert (X + Y = 26, "Addition is incorrect");
      end Test_Simple_Add;

      -- Register test routines to call
      procedure Register_Tests (T: in out Money_Test) is
         use AUnit.Test_Cases.Registration;
      begin
         -- Repeat for each test routine:
         Register_Routine (T, Test_Simple_Add'Access, "Test Addition");
      end Register_Tests;

      -- Identifier of test case

      function Name (T: Money_Test) return Test_String is
      begin
         return Format ("Money Tests");
      end Name;

      function Location (T : Money_Test) return Tested_Location is
      begin
         return
           (Tested_File   => Format ("money_tests-suite.adb"),
            Tested_Line   => 16,
            Tested_Column => 4,
            Tested_Name => null);
      end Location;

      function Suffix (T : Money_Test) return Test_Suffix_Access is
      begin
         return null;
      end Suffix;

      function Package_Name (T : Money_Test) return Message_String is
      begin
         return Format ("Money_Tests.Money_Test_Test_Data.Money_Test_Tests");
      end Package_Name;

   end Money_Tests;


.. index:: AUnit.Test_Caller generic package

.. _AUnit-Test_Caller:

AUnit.Test_Caller
=================

.. index:: AUnit.Test_Fixtures.Test_Fixture type

``Test_Caller`` is a generic package that is used with
``AUnit.Test_Fixtures.Test_Fixture``. ``Test_Fixture`` is a very
simple type that provides only the ``Set_Up`` and ``Tear_Down``
procedures. This type is meant to contain a set of user-defined test
routines, all using the same set up and tear down mechanisms. Once those
routines are defined, the ``Test_Caller`` package is used to incorporate them
directly into a test suite.

With our money example, the ``Test_Fixture`` is:

.. code-block:: ada

   with AUnit.Test_Fixtures;
   package Money_Tests is
      type Money_Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

      procedure Test_Simple_Add (T : in out Money_Test);

   end Money_Tests;

The test suite (see :ref:`Suite`) calling the test cases created from
this Test_Fixture is:

.. code-block:: ada

   with AUnit.Test_Suites;
   package Money_Suite is
      function Suite return AUnit.Test_Suites.Access_Test_Suite;
   end Money_Suite;

Here is the corresponding body:

.. code-block:: ada

   with AUnit.Test_Caller;
   with Money_Tests;

   package body Money_Suite is

      package Money_Caller is new AUnit.Test_Caller
         (Money_Tests.Money_Test);

      function Suite return Aunit.Test_Suites.Access_Test_Suite is
         Ret : AUnit.Test_Suites.Access_Test_Suite :=
         AUnit.Test_Suites.New_Suite;
      begin
         Ret.Add_Test
            (Money_Caller.Create
               (Name         => "Money Test : Test Addition",
                Package_Name => "Money_Test.Suite",
                Test_File    => "money_test_suite.ads",
                Location     => (
                    Tested_File   => new String'("money_test.adb"),
                    Tested_Line   => 22,
                    Tested_Column => 4,
                    Tested_Name =>  new String'("Test Addition")
                ),
                Suffix       => null,
                Test         => Money_Tests.Test_Simple_Add'Access));
         return Ret;
      end Suite;

   end Money_Suite;

Note that ``New_Suite`` and ``Create`` are fully compatible with
limited run-time libraries (in particular, those without dynamic allocation support).
However, for non-native run-time libraries, you cannot extend
``Test_Fixture`` with a controlled component.

You can find a compilable example of
``AUnit.Test_Caller`` usage in the AUnit installation
directory: :samp:`{<aunit-root>}/share/examples/aunit/test_caller/` or from the
source distribution :samp:`aunit-{<version>}-src/examples/test_caller/`.
