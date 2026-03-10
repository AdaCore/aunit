.. _Fixture:

*******
Fixture
*******

.. index:: 
   see: Test fixture; Fixture

.. index:: Fixture

Tests need to run against the background of a set of known entities.
This set is called a *test fixture*. When you are
writing tests you will often find that you spend more time writing
code to set up the fixture than you do in actually testing values.

You can make writing fixture code easier by sharing it.
Often you will be able to use the same fixture for several different
tests. Each case will send slightly different messages or parameters to the
fixture and will check for different results.

When you have a common fixture, here is what you do:

* Create a *Test Case* package as in previous section.

* Declare variables or components for elements of the fixture either
  as part of the test case type or in the package body.

* According to the Test_Case type used, override its ``Set_Up``
  and/or ``Set_Up_Case`` subprogram:

  .. index:: AUnit.Simple_Test_Cases.Set_Up procedure 

  * ``AUnit.Simple_Test_Cases``: ``Set_Up`` is called before
    ``Run_Test``.

  .. index:: AUnit.Test_Cases.Set_Up procedure 
  .. index:: AUnit.Test_Cases.Set_Up_Case procedure 

  * ``AUnit.Test_Cases``: ``Set_Up`` is called before each test
    routine while ``Set_Up_Case`` is called once before the routines are run.

  .. index:: AUnit.Test_Fixtures.Set_Up_Case procedure 

  * ``AUnit.Test_Fixtures``: ``Set_Up`` is called before each
    test case created using ``Aunit.Test_Caller``.

* You can also override ``Tear_Down`` and/or
  ``Tear_Down_Case`` that are executed after the test is run.


For example, to write several test cases that want to work with
different combinations of 12 Euros, 14 Euros, and 26 US Dollars, first
create a fixture. The package spec is now:

.. code-block:: ada

   with AUnit; use AUnit;
   package Money_Tests is
      use Test_Results;

      type Money_Test is new Test_Cases.Test_Case with null record;

      procedure Register_Tests (T: in out Money_Test);
      -- Register routines to be run

      function Name (T : Money_Test) return Test_String;
      -- Provide name identifying the test case

      procedure Set_Up (T : in out Money_Test);
      --  Set up performed before each test routine

      -- Test Routines:
      procedure Test_Simple_Add (T : in out Test_Cases.Test_Case'Class);
   end Money_Tests;

The body becomes:

.. code-block:: ada

   package body Money_Tests is
      use Assertions;

      -- Fixture elements

      EU_12, EU_14 : Euro;
      US_26        : US_Dollar;

      -- Preparation performed before each routine

      procedure Set_Up (T: in out Money_Test) is
      begin
         EU_12 := 12; EU_14 := 14;
         US_26 := 26;
      end Set_Up;

      procedure Test_Simple_Add (T : in out Test_Cases.Test_Case'Class) is
         X, Y : Some_Currency;
      begin
         Assert (EU_12 + EU_14 /= US_26, 
                 "US and EU currencies not differentiated");
      end Test_Simple_Add;

      -- Register test routines to call
      procedure Register_Tests (T: in out Money_Test) is
         use Test_Cases.Registration;
      begin
         -- Repeat for each test routine:
         Register_Routine (T, Test_Simple_Add'Access, "Test Addition");
      end Register_Tests;

      -- Identifier of test case
      function Name (T: Money_Test) return Test_String is
      begin
         return Format ("Money Tests");
      end Name;

   end Money_Tests;

Once you have the fixture in place, you can write as many test
routines as you like. Calls to ``Set_Up`` and ``Tear_Down``
bracket the invocation of each test routine.

Once you have several test cases, organize them into a Suite.

.. index:: AUnit.Test_Fixtures

You can find a compilable example of fixture set up using
``AUnit.Test_Fixtures`` in your AUnit installation directory: 
:samp:`{<aunit-root>}/share/examples/aunit/test_fixture/` 
or from the AUnit source distribution 
:samp:`aunit-{<version>}-src/examples/test_fixture/`.

