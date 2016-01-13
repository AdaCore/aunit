.. index:: Test organization

.. _Test_Organization:

*****************
Test Organization
*****************


.. _General_considerations:

General considerations
======================

This section will discuss an approach to organizing an AUnit test harness,
considering some possibilities offered by Ada language features.

The general idea behind this approach to test organization is that making the
test case a child of the unit under test gives some useful facilities.
The test case gains visibility to the private part of the unit under test.
This offers a more 'white box' approach to examining the state of the unit
under test than would, for instance, accessor functions defined in a separate
fixture that is a child of the unit under test. Making the test case a child of
the unit under test also provides a way to make the test case share certain
characteristics of the unit under test.  For instance, if the unit under test
is generic, then any child package (here the test case) must be also generic:
any instantiation of the parent package will require an instantiation of the
test case in order to accomplish its aims.

Another useful concept is matching the test case type to that of the unit
under test, for example:

* When testing a generic package, the test package should also be
  generic.
* When testing a tagged type, then test routines should be
  dispatching, and the test case type for a derived tagged type should be a
  derivation of the test case type for the parent.

Maintaining such similarity of properties between the test case and unit under
test can facilitate the testing of units derived in various ways.

The following sections will concentrate on applying these concepts to
the testing of tagged type hierarchies and to the testing of generic units.

A full example of this kind of test organization is available in the AUnit
installation directory:
:samp:`{<AUnit-root>}/share/examples/aunit/calculator`, or
from the AUnit source distribution
:samp:`aunit-{<version>}-src/examples/calculator`.

.. index:: OOP considerations (in test organization)

.. _OOP_considerations:

OOP considerations
==================

When testing a hierarchy of tagged types, one will often want to run tests
for parent types against their derivations without rewriting those tests.

We will illustrate some of the possible solutions available in AUnit,
using the following simple example that we want to test:

First we consider a ``Root`` package defining the ``Parent``
tagged type, with two procedures ``P1`` and ``P2``.

.. code-block:: ada

   package Root is
      type Parent is tagged private;

      procedure P1 (P : in out Parent);
      procedure P2 (P : in out Parent);
   private
      type Parent is tagged record
         Some_Value : Some_Type;
      end record;
   end Root;

We will also consider a derivation from type ``Parent``:

.. code-block:: ada

   with Root;
   package Branch is
      type Child is new Root.Parent with private;

      procedure P2 (C : in out Child);
      procedure P3 (C : in out Child);
   private
      type Child is new Root.Parent with null record;
   end Branch;

Note that ``Child`` retains the parent implementation of ``P1``,
overrides ``P2`` and adds ``P3``. Its test will override
``Test_P2`` when we override ``P2`` (not necessary, but certainly
possible).

.. index:: AUnit.Test_Fixtures.Test_Fixture type

Using AUnit.Test_Fixtures
-------------------------

Using type ``Test_Fixture`, we first test ``Parent`` using the
following test case:

.. code-block:: ada

   with AUnit; use AUnit;
   with AUnit.Test_Fixtures; use AUnit.Test_Fixtures;

   --  We make this package a child package of Parent so that it can have
   --  visibility to its private part
   package Root.Tests is

      type Parent_Access is access all Root.Parent'Class;

      --  Reference an object of type Parent'Class in the test object, so
      --  that test procedures can have access to it.
      type Parent_Test is new Test_Fixture with record
         Fixture : Parent_Access;
      end record;

      --  This will initialize P.
      procedure Set_Up (P : in out Parent_Test);

      --  Test routines. If derived types are declared in child packages,
      --  these can be in the private part.
      procedure Test_P1 (P : in out Parent_Test);
      procedure Test_P2 (P : in out Parent_Test);

   end Root.Tests;

.. code-block:: ada

   package body Root.Tests is

      Fixture : aliased Parent;

      --  We set Fixture in Parent_Test to an object of type Parent.
      procedure Set_Up (P : in out Parent_Test) is
      begin
         P.Fixture := Parent_Access (Fixture'Access);
      end Set_Up;

      --  Test routines: References to the Parent object are made via
      --  P.Fixture.all, and are thus dispatching.
      procedure Test_P1 (P : in out Parent_Test) is ...;
      procedure Test_P2 (P : in out Parent_Test) is ...;

end Root.Tests;

The associated test suite will be:

.. code-block:: ada

   with AUnit.Test_Caller;
   with Root.Tests;

   package body Root_Suite is
      package Caller is new AUnit.Test_Caller with (Root.Tests.Parent_Test);

      function Suite return AUnit.Test_Suites.Access_Test_Suite is
         Ret : Access_Test_Suite := AUnit.Test_Suites.New_Suite;
      begin
         AUnit.Test_Suites.Add_Test
            (Ret, Caller.Create ("Test Parent : P1", Root.Tests.Test_P1'Access));
         AUnit.Test_Suites.Add_Test
            (Ret, Caller.Create ("Test Parent : P2", Root.Tests.Test_P2'Access));
         return Ret;
      end Suite;
   end Root_Suite;

Now we define the test suite for the ``Child`` type. To do this,
we inherit a test fixture from ``Parent_Test``,
overriding the ``Set_Up`` procedure to initialize ``Fixture`` with
a ``Child`` object. We also override ``Test_P2`` to adapt it
to the new implementation. We define a new ``Test_P3`` to test
``P3``. And we inherit ``Test_P1``, since ``P1`` is unchanged.

.. code-block:: ada

   with Root.Tests; use Root.Tests;
   with AUnit; use AUnit;
   with AUnit.Test_Fixtures; use AUnit.Test_Fixtures;

   package Branch.Tests is

      type Child_Test is new Parent_Test with null record;

      procedure Set_Up (C : in out Child_Test);

      --  Test routines:
      --  Test_P2 is overridden
      procedure Test_P2 (C : in out Child_Test);
      --  Test_P3 is new
      procedure Test_P3 (C : in out Child_Test);

   end Branch.Tests;

.. code-block:: ada

   package body Branch.Tests is
      use Assertions;

      Fixture : Child;
      --  This could also be a field of Child_Test

      procedure Set_Up (C : in out Child_Test) is
      begin
         --  The Fixture for this test will now be a Child
         C.Fixture := Parent_Access (Fixture'Access);
      end Set_Up;

      --  Test routines:
      procedure Test_P2 (C : in out Child_Test) is ...;
      procedure Test_P3 (C : in out Child_Test) is ...;

   end Branch.Tests;

The suite for Branch.Tests will now be:

.. code-block:: ada

   with AUnit.Test_Caller;
   with Branch.Tests;

   package body Branch_Suite is
      package Caller is new AUnit.Test_Caller with (Branch.Tests.Parent_Test);

      --  In this suite, we use Ada 2005 distinguished receiver notation to
      --  simplify the code.

      function Suite return Access_Test_Suite is
         Ret : Access_Test_Suite := AUnit.Test_Suites.New_Suite;
      begin
         --  We use the inherited Test_P1. Note that it is
         --  Branch.Tests.Set_Up that will be called, and so Test_P1 will be run
         --  against an object of type Child
         Ret.Add_Test
            (Caller.Create ("Test Child : P1", Branch.Tests.Test_P1'Access));
         --  We use the overridden Test_P2
         Ret.Add_Test
            (Caller.Create ("Test Child : P2", Branch.Tests.Test_P2'Access));
         --  We use the new Test_P2
         Ret.Add_Test
            (Caller.Create ("Test Child : P3", Branch.Tests.Test_P3'Access));
         return Ret;
      end Suite;
   end Branch_Suite;

Using AUnit.Test_Cases
----------------------

.. index:: AUnit.Test_Cases.Test_Case type

Using an ``AUnit.Test_Cases.Test_Case`` derived type, we obtain the
following code for testing ``Parent``:

.. code-block:: ada

   with AUnit; use AUnit;
   with AUnit.Test_Cases;
   package Root.Tests is

      type Parent_Access is access all Root.Parent'Class;

      type Parent_Test is new AUnit.Test_Cases.Test_Case with record
         Fixture : Parent_Access;
      end record;

      function Name (P : Parent_Test) return Message_String;
      procedure Register_Tests (P : in out Parent_Test);

      procedure Set_Up_Case (P : in out Parent_Test);

      --  Test routines. If derived types are declared in child packages,
      --  these can be in the private part.
      procedure Test_P1 (P : in out Parent_Test);
      procedure Test_P2 (P : in out Parent_Test);

   end Root.Tests;

.. index:: AUnit.Test_Cases.Specific_Test_Case_Registration generic package

The body of the test case will follow the usual pattern, declaring one or
more objects of type ``Parent``, and executing statements in the
test routines against them.  However, in order to support dispatching to
overriding routines of derived test cases, we need to introduce class-wide
wrapper routines for each primitive test routine of the parent type that
we anticipate may be overridden. Instead of registering the parent's
overridable primitive operations directly using ``Register_Routine``,
we register the wrapper using ``Register_Wrapper``. This latter routine
is exported by instantiating
``AUnit.Test_Cases.Specific_Test_Case_Registration`` with the actual
parameter being the parent test case type.

.. code-block:: ada

   with AUnit.Assertions; use AUnit.Assertions
   package body Root.Tests is

      --  Declare class-wide wrapper routines for any test routines that will be
      --  overridden:
      procedure Test_P1_Wrapper (P : in out Parent_Test'Class);
      procedure Test_P2_Wrapper (P : in out Parent_Test'Class);

      function Name (P : Parent_Test) return Message_String is ...;

      --  Set the fixture in P
      Fixture : aliased Parent;

      procedure Set_Up_Case (P : in out Parent_Test) is
      begin
         P.Fixture := Parent_Access (Fixture'Access);
      end Set_Up_Case;

      --  Register Wrappers:
      procedure Register_Tests (P : in out Parent_Test) is
         package Register_Specific is
            new Test_Cases.Specific_Test_Case_Registration (Parent_Test);
         use Register_Specific;
      begin
         Register_Wrapper (P, Test_P1_Wrapper'Access, "Test P1");
         Register_Wrapper (P, Test_P2_Wrapper'Access, "Test P2");
      end Register_Tests;

      --  Test routines:
      procedure Test_P1 (P : in out Parent_Test) is ...;
      procedure Test_P2 (C : in out Parent_Test) is ...;

      --  Wrapper routines. These dispatch to the corresponding primitive
      --  test routines of the specific types.
      procedure Test_P1_Wrapper (P : in out Parent_Test'Class) is
      begin
         Test_P1 (P);
      end Test_P1_Wrapper;

      procedure Test_P2_Wrapper (P : in out Parent_Test'Class) is
      begin
         Test_P2 (P);
      end Test_P2_Wrapper;

   end Root.Tests;

The code for testing the `Child` type will now be:

.. code-block:: ada

   with Parent_Tests; use Parent_Tests;
   with AUnit; use AUnit;
   package Branch.Tests is

      type Child_Test is new Parent_Test with private;

      function Name (C : Child_Test) return Message_String;
      procedure Register_Tests (C : in out Child_Test);

      --  Override Set_Up_Case so that the fixture changes.
      procedure Set_Up_Case (C : in out Child_Test);

      --  Test routines:
      procedure Test_P2 (C : in out Child_Test);
      procedure Test_P3 (C : in out Child_Test);

   private
      type Child_Test is new Parent_Test with null record;
   end Branch.Tests;

.. code-block:: ada

   with AUnit.Assertions; use AUnit.Assertions;
   package body Branch.Tests is

      --  Declare wrapper for Test_P3:
      procedure Test_P3_Wrapper (C : in out Child_Test'Class);

      function Name (C : Child_Test) return Test_String is ...;

      procedure Register_Tests (C : in out Child_Test) is
         package Register_Specific is
            new Test_Cases.Specific_Test_Case_Registration (Child_Test);
         use Register_Specific;
      begin
         -- Register parent tests for P1 and P2:
         Parent_Tests.Register_Tests (Parent_Test (C));

         -- Repeat for each new test routine (Test_P3 in this case):
         Register_Wrapper (C, Test_P3_Wrapper'Access, "Test P3");
      end Register_Tests;

      --  Set the fixture in P
      Fixture : aliased Child;
      procedure Set_Up_Case (C : in out Child_Test) is
      begin
         C.Fixture := Parent_Access (Fixture'Access);
      end Set_Up_Case;

      --  Test routines:
      procedure Test_P2 (C : in out Child_Test) is ...;
      procedure Test_P3 (C : in out Child_Test) is ...;

      --  Wrapper for new routine:
      procedure Test_P3_Wrapper (C : in out Child_Test'Class) is
      begin
         Test_P3 (C);
      end Test_P3_Wrapper;

   end Branch.Tests;

Note that inherited and overridden tests do not need to be explicitly
re-registered in derived test cases - one just calls the parent version of
``Register_Tests``. If the application tagged type hierarchy is organized
into parent and child units, one could also organize the test cases into a
hierarchy that reflects that of the units under test.

.. index:: Generic units (testing)

.. _Testing_generic_units:

Testing generic units
=====================

When testing generic units, one would like to apply the same generic tests
to all instantiations in an application.  A simple approach is to make the
test case a child package of the unit under test (which then must also be
generic).

For instance, suppose the generic unit under test is a package (it could
be a subprogram, and the same principle would apply):

.. code-block:: ada

   generic
      -- Formal parameter list
   package Template is
      -- Declarations
   end Template;

The corresponding test case would be:

.. code-block:: ada

   with AUnit; use AUnit;
   with AUnit.Test_Fixtures;
   generic
   package Template.Gen_Tests is

      type Template_Test is new AUnit.Test_Fixtures.Test_Fixture with ...;

      --  Declare test routines

   end Template.Gen_Tests;

The body will follow the usual patterns with the fixture based on the
parent package ``Template``. Note that due to an Ada AI, accesses to
test routines, along with the test routine specifications, must be defined
in the package specification rather than in its body.

Instances of ``Template`` will automatically define the ``Tests`` child
package that can be directly instantiated as follows:

.. code-block:: ada

   with Template.Gen_Test;
   with Instance_Of_Template;
   package Instance_Of_Template.Tests is new Instance_Of_Template.Gen_Test;

The instantiated test case objects are added to a suite in the usual manner.

