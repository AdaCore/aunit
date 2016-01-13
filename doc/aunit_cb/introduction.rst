.. _Introduction:

************
Introduction
************

This is a short guide for using the AUnit test framework.
AUnit is an adaptation of the Java :index:`JUnit` (Kent Beck, Erich Gamma) and C++
:index:`CppUnit` (M. Feathers, J. Lacoste, E. Sommerlade, B. Lepilleur, B. Bakker,
S. Robbins) unit test frameworks for Ada code.


What's new in AUnit 3
=====================

AUnit 3 brings several enhancements over AUnit 2 and AUnit 1:


* Removal of the genericity of the AUnit framework, making the AUnit 3
  API as close as possible to AUnit 1.

* Emulates dynamic memory management for limited run-time profiles.

* Provides a new XML reporter, and changes harness invocation to support
  easy switching among text, XML and customized reporters.

* Provides new tagged types ``Simple_Test_Case``, ``Test_Fixture`` and ``Test_Caller``
  that correspond to CppUnit's ``TestCase``, ``TestFixture`` and ``TestCaller`` classes.

.. index:: ZFP profile

.. index:: setjmp/longjmp

* Emulates exception propagation for restricted run-time profiles
  (e.g. ZFP), by using the  gcc builtin `setjmp` / `longjmp` mechanism.

* Reports the source location of an error when possible.


Typographic conventions
=======================

.. index:: <version> notational convention

For notational convenience, `<version>` will be used throughout
this document to stand for the AUnit product version number.
For example, aunit-*<version>*-src expands to aunit-|version|-src.


Examples
========

With this version, we have provided new examples illustrating the enhanced
features of the framework. These examples are in the AUnit
installation directory:
:file:`<aunit-root>/share/examples/aunit`, and are also available in the source
distribution :samp:`aunit-{<version>}-src/examples`.

The following examples are provided:

* simple_test: shows use of AUnit.Simple_Test_Cases
  (see :ref:`AUnit.Simple_Test_Cases<AUnit-Simple_Test_Cases>`).
* test_caller: shows use of AUnit.Test_Caller (see :ref:`AUnit.Test_Caller<AUnit-Test_Caller>`).
* test_fixture: example of a test fixture (see :ref:`Fixture`).
* liskov: This suite tests conformance to the Liskov Substitution Principle
  of a pair of simple tagged types. (see :ref:`OOP_considerations`)
* failures: example of handling and reporting failed tests
  (see :ref:`Reporting`).
* calculator: a full example of test suite organization.

Note about limited run-time libraries
=====================================

AUnit allows a great deal of flexibility for the structure of test cases,
suites and harnesses.  The templates and examples given in this document
illustrate how to use AUnit while staying within the constraints of the
GNAT Pro restricted and Zero Footprint (ZFP) run-time libraries. Therefore,
they avoid the use of dynamic allocation and some other features that would
be outside of the profiles corresponding to these libraries. Tests targeted
to the full Ada run-time library need not comply with these constraints.

Thanks
======

This document is adapted from the JUnit and CppUnit Cookbooks documents
contained in their respective release packages.

.. |c-cedilla-lc| unicode:: 0xE7
   :trim:

Special thanks to Fran |c-cedilla-lc| ois Brun of Thales Avionics for his ideas about
support for OOP testing.

