.. _Reporting:

*********
Reporting
*********

.. index:: Reporting
.. index:: AUnit.Reporter.Text.Text_Reporter type
.. index:: AUnit.Reporter.XML.XML_Reporter type
.. index:: AUnit.Run.Test_Runner

Test results can be reported using several `Reporters`. By default, two
reporters are available in AUnit:
``AUnit.Reporter.Text.Text_Reporter`` and
``AUnit.Reporter.XML.XML_Reporter``. The first one is a simple
console reporting routine, while the second one outputs the result using
an XML format. These are invoked when the ``Run`` routine of an
instantiation of ``AUnit.Run.Test_Runner`` is called.

.. index:: AUnit.Reporter.Reporter

New reporters can be created using children of
``AUnit.Reporter.Reporter``.

The Reporter is selected by specifying it when calling ``Run``:

.. code-block:: ada

   with A_Suite;
   with AUnit.Run;
   with AUnit.Reporter.Text;

   procedure My_Tests is
      procedure Run is new AUnit.Run.Test_Runner (A_Suite.Suite);
      Reporter : AUnit.Reporter.Text.Text_Reporter;
   begin
      Run (Reporter);
   end My_Tests;

.. index:: Test_Result type

The final report is output once all tests have been run, so that they can be
grouped depending on their status (passed or fail). If you need to output the
tests as they are run, you should consider extending the `Test_Result`
type and do some output every time a success or failure is registered.

Text output
===========

Here is an example where the test harness runs 4 tests, one reporting an
assertion failure, one reporting an unexpected error (exception):


::

  --------------------

     Total Tests Run: 4

     Successful Tests: 2
        Test addition
        Test subtraction

     Failed Assertions: 1

        Test addition (failure expected)
              Test should fail this assertion, as 5+3 /= 9
              at math-test.adb:29

     Unexpected Errors: 1

        Test addition (error expected)
              CONSTRAINT_ERROR

  Time: 2.902E-4 seconds
  
.. index:: Colors (in report output)

This reporter can optionally use colors (green to report success, red to report
errors). Since not all consoles support it, this is off by default, but you can
call ``Set_Use_ANSI_Colors`` to activate support for colors.

.. index:: XML output

XML output
==========

Following is the same harness run using XML output. The XML format used
matches the one used by :index:`CppUnit`.

.. index:: UTF-8 character encoding

Note that text set in the `Assert` subprograms or as test case names should
be compatible with utf-8 character encoding, or the XML will not be
correctly formatted.


::

  <?xml version='1.0' encoding='utf-8' ?>
  <TestRun elapsed='1.107E-4'>
    <Statistics>
      <Tests>4</Tests>
      <FailuresTotal>2</FailuresTotal>
      <Failures>1</Failures>
      <Errors>1</Errors>
    </Statistics>
    <SuccessfulTests>
      <Test>
        <Name>Test addition</Name>
      </Test>
      <Test>
        <Name>Test subtraction</Name>
      </Test>
    </SuccessfulTests>
    <FailedTests>
      <Test>
        <Name>Test addition (failure expected)</Name>
        <FailureType>Assertion</FailureType>
        <Message>Test should fail this assertion, as 5+3 /= 9</Message>
        <Location>
          <File>math-test.adb</File>
          <Line>29</Line>
        </Location>
      </Test>
      <Test>
        <Name>Test addition (error expected)</Name>
        <FailureType>Error</FailureType>
        <Message>CONSTRAINT_ERROR</Message>
      </Test>
    </FailedTests>
  </TestRun>
  

