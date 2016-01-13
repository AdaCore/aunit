.. _Suite:

*****
Suite
*****


Creating a Test Suite
=====================

How do you run several test cases at once?

.. index::
   see:  Test_Suite; AUnit.Test_Suites.Test_Suite
   see:  Test_Suites; AUnit.Test_Suites

.. index:: AUnit.Test_Suites package
.. index:: AUnit.Test_Suites.Test_Suite type


As soon as you have two tests, you'll want to run them together.
You could run the tests one at a time yourself, but you would quickly
grow tired of that. Instead, AUnit provides an object, ``Test_Suite``,
that runs any number of test cases together.

To create a suite of two test cases and run them together, first create
a test suite:

.. code-block:: ada

   with AUnit.Test_Suites;
   package My_Suite is
      function Suite return AUnit.Test_Suites.Access_Test_Suite;
   end My_Suite;

.. code-block:: ada

   --  Import tests and sub-suites to run
   with Test_Case_1, Test_Case_2;
   package body My_Suite is
      use AUnit.Test_Suites;

      -- Statically allocate test suite:
      Result : aliased Test_Suite;

      --  Statically allocate test cases:
      Test_1 : aliased Test_Case_1.Test_Case;
      Test_2 : aliased Test_Case_2.Test_Case;

      function Suite return Access_Test_Suite is
      begin
         Add_Test (Result'Access, Test_Case_1'Access);
         Add_Test (Result'Access, Test_Case_2'Access);
         return Result'Access;
      end Suite;
   end My_Suite;

.. index:: AUnit.Test_Suites.New_Suite, AUnit.Memory.Utils.Gen_Alloc

Instead of statically allocating test cases and suites,
you can also use ``AUnit.Test_Suites.New_Suite`` and/or
``AUnit.Memory.Utils.Gen_Alloc``. These routines emulate dynamic
memory management (see :ref:`Using_AUnit_with_Restricted_Run-Time_Libraries`).
Similarly, if you know that the tests will always be executed for a run-time
profile that supports dynamic memory management, you can allocate these
objects directly with the Ada ``new`` operation.

The harness is:

.. code-block:: ada

   with My_Suite;
   with AUnit.Run;
   with AUnit.Reporter.Text;

   procedure My_Tests is
      procedure Run is new AUnit.Run.Test_Runner (My_Suite.Suite);
      Reporter : AUnit.Reporter.Text.Text_Reporter;
   begin
      Run (Reporter);
   end My_Tests;


.. index:: Composition of test suites

Composition of Suites
=====================

Typically, one will want the flexibility to execute a complete set
of tests, or some subset of them. In order to facilitate this, we can
compose both suites and test cases, and provide a harness for any given
suite:

.. code-block:: ada

   -- Composition package:
   with AUnit; use AUnit;
   package Composite_Suite is
      function Suite return Test_Suites.Access_Test_Suite;
   end Composite_Suite;

   --  Import tests and suites to run
   with This_Suite, That_Suite;
   with AUnit.Tests;
   package body Composite_Suite is
      use Test_Suites;

      --  Here we dynamically allocate the suite using the New_Suite function
      --  We use the 'Suite' functions provided in This_Suite and That_Suite
      --  We also use Ada 2005 distinguished receiver notation to call Add_Test

      function Suite return Access_Test_Suite is
         Result : Access_Test_Suite := AUnit.Test_Suites.New_Suite;
      begin
         Result.Add_Test (This_Suite.Suite);
         Result.Add_Test (That_Suite.Suite);
         return Result;
      end Suite;
   end Composite_Suite;

The harness remains the same:

.. code-block:: ada

   with Composite_Suite;
   with AUnit.Run;

   procedure My_Tests is
      procedure Run is new AUnit.Run.Test_Runner (Composite_Suite.Suite);
      Reporter : AUnit.Reporter.Text.Text_Reporter;
   begin
      Run (Reporter);
   end My_Tests;

As can be seen, this is a very flexible way of composing test cases
into execution runs: any combination of test cases and sub-suites can
be collected into a suite.

