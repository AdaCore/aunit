.. _Reporting:

*********
Reporting
*********

.. index:: Reporting
.. index:: AUnit.Reporter.Text.Text_Reporter type
.. index:: AUnit.Reporter.GNATtest.GNATtest_Reporter type
.. index:: AUnit.Reporter.Junit.Junit_Reporter type
.. index:: AUnit.Reporter.XML.XML_Reporter type
.. index:: AUnit.Reporter.XML_DEPRECATED.XML_DEPRECATED_Reporter type
.. index:: AUnit.Run.Test_Runner

Test results can be reported using several `Reporters`. By default, four 
reporters are available in AUnit:
- ``AUnit.Reporter.Text.Text_Reporter`` : A simple console reporting
routine.
- ``AUnit.Reporter.GNATtest.GNATtest_Reporter`` : Base reporter for GNATtest
which report test in a human readable manner.
- ``AUnit.Reporter.JUnit.JUnit_Reporter`` : Output results using JUnit XML 
format.
- ``AUnit.Reporter.XML_DEPRECATED.XML_DEPRECATED_Reporter`` : previous version of the now updated XML_Reporter. This format is now deprecated, consider using XML_Reporter instead.

These are invoked when the ``Run`` routine of an
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

Test Examples
=============

The reporter outputs you will see in the next sections are generated using two test files :

- foo.adb

.. code-block:: ada 

    package body Foo is
        procedure Test_Not_Equal is
        begin
          AUnit.Assertions.Assert (5 = 3, "Value not equal");
        end Test_Not_Equal;


        procedure Test_Sub_Zero_Natural is
           Result : Natural := 0;
        begin
           Put_Line ("This test will crash");
           Put_Line (Current_Err, "This test will crash but on stderr");
           AUnit.Assertions.Assert
             (Natural (Result - 5) = 5, "This test should crash here");
        end Test_Sub_Zero_Natural;
    end Foo;

- bar.adb

.. code-block:: ada 
   
    package body Bar is
        procedure Test_Not_Implemented is
        begin
          AUnit.Assertions.Assert
            (Gnattest_Generated.Default_Assert_Value, "Test not implemented.");
        end Test_Not_Equal;


        procedure Test_Always_True is
        begin
          AUnit.Assertions.Assert (True, "Should always pass");
        end Test_Sub_Zero_Natural;
    end Bar;


In total the four tests will be run : 1 test passes successfully, 2 tests reports an assertion 
failure and the last one will report an unexpected error (exception).

Text output
===========

This reporter output each test separately, prefixed by the status of the tests (``OK``, ``FAIL`` or ``ERROR``), 
followed by the location of the tested function.
If the test failed, the reporter will print the message of the failed assertion and the line where 
it was raised. 
If the test encountered an error, the reporter will print the ``error name``, the ``exception message`` and 
then its ``Traceback``.

Once the reporter has outputted all the tests, it will output a summary of the sessions with the following information:

    - ``Total Tests Run`` : the count of all the tests that were executed this session.
    - ``Successful Tests`` : the number of tests that were successfully executed this session.
    - ``Failed Assertions``: the number of tests that failed this session.
    - ``Unexpected Errors``: the number of tests that crashed this session. 
    - ``Cumulative Time``: the time it took to run all the tests.

::

  --------------------
    OK bar.ads:9:5:
    
    FAIL foo.ads:7:4:
            Value not equal
            at foo-test_data-tests.adb:46
    FAIL bar.ads:7:4:
            Test not implemented.
            at bar-test_data-tests.adb:44
    
    ERROR foo.ads:9:5:
            CONSTRAINT_ERROR
            Exception Message: foo-test_data-tests.adb:65 range check failed
            Traceback:
                [....]
    
    
    Total Tests Run:   4
    Successful Tests:  1
    Failed Assertions: 2
    Unexpected Errors: 1
    Cumulative Time: 1.005306 sec.

If the ``AUnit_Options.Test_Case_Timer`` was set, each line will also output the test duration in this format :
::

    OK bar.ads:9:5: (in 0.978468 sec.)

It is also possible to pass the test case name when creating the ``Test_Case`` object in which case the reporter will output it in this format : 
::

    OK bar.ads:9:5: (Bar.Test_Always_True)

.. index:: Colors (in report output)

This reporter can optionally use colors (green to report success, red to report
errors). Since not all consoles support it, this is off by default, but you can
call ``Set_Use_ANSI_Colors`` to activate support for colors.

.. index:: GNATtest output

GNATtest output
================

This reporter output is similar to the ``text reporter`` above but in a more compressed format.

For each test, the reporter outputs the location of the tested function and then ``info: corresponding test PASSED`` for successful tests. 
For failure, it outputs ``error: corresponding test FAILED:`` followed by the assertion's ``failed message`` and then the ``location of the assertion``. 
For error, it outputs ``error: corresponding test CRASHED:`` followed by the ``error name``, the ``error message`` and then on the next lines the ``Traceback``.

At the end of each line representing an unsuccessful test, the location of the gnattest generated test case is added.

::

  --------------------
    bar.ads:9:5: info: corresponding test PASSED
    foo.ads:7:4: error: corresponding test FAILED: Value not equal (foo-test_data-tests.adb:45)
    bar.ads:7:4: error: corresponding test FAILED: Test not implemented. (bar-test_data-tests.adb:44)
    foo.ads:9:5: error: corresponding test CRASHED: CONSTRAINT_ERROR : foo-test_data-tests.adb:63 range check failed
      Traceback:
        [....]


Once again, if the ``AUnit_Options.Test_Case_Timer`` was set, each line will also output the test duration in this format :
::

    bar.ads:9:5: info: corresponding test PASSED (0.000976946s)

If the test case name was passed to the ``Test_Case`` object, the reporter will additionaly output it in this format:
::

    bar.ads:9:5: (Bar.Test_Always_True) info: corresponding test PASSED

.. index:: JUnit XML output

JUnit XML output
================

It is also possible use a reporter to output a JUnit XML report.

<testsuites>
------------

The top level element of the document, used to hold a set of ``<testsuite>`` object.

+-----------+----------------------------------+---------+
| Attribute | Description                      | Type    |
+===========+==================================+=========+
| skipped   | The total number of skipped test | Integer |
|           | for the whole test session       |         |
+-----------+----------------------------------+---------+
| tests     | The total number of test run     | Integer | 
|           | in this test session             |         |
+-----------+----------------------------------+---------+
| failures  | The total number of failed test  | Integer |
|           | for the whole test session       |         |
+-----------+----------------------------------+---------+
| errors    | The total number of crashed test | Integer |
|           | for the whole test session       |         |
+-----------+----------------------------------+---------+
| time      | The total time to run all        | Float   |
|           | the tests                        |         |
+-----------+----------------------------------+---------+

+------------------+-------------------------+-------------+
| Child            | Description             | Cardinality |
+==================+=========================+=============+
| `\<testsuite\>`_ | Contains a set of tests | 0..*        |
+------------------+-------------------------+-------------+

<testsuite>
-----------

This element hold the tests for a single ada package.

+-----------+-----------------------------+---------+
| Attribute | Description                 | Type    |
+===========+=============================+=========+
| name      | The name of the ada package | String  |
|           | the tests are located in    |         |
+-----------+-----------------------------+---------+
| skipped   | The number of skipped test  | Integer |
|           | in this package             |         |
+-----------+-----------------------------+---------+
| tests     | The number of test runned   | Integer | 
|           | in this package             |         |
+-----------+-----------------------------+---------+
| failures  | The number of failed test   | Integer |
|           | in this package             |         |
+-----------+-----------------------------+---------+
| errors    | The number of crashed test  | Integer |
|           | in this package             |         |
+-----------+-----------------------------+---------+
| time      | The total time to run all   | Float   |
|           | the tests in this package   |         |
+-----------+-----------------------------+---------+


+-----------------+-------------------------+-------------+
| Child           | Description             | Cardinality |
+=================+=========================+=============+
| `\<testcase\>`_ | Describe the result of  | 0..*        |
|                 | specific test.          |             |
+-----------------+-------------------------+-------------+

<testcase>
----------

This element describe a test result.

+-----------+-----------------------------+---------+
| Attribute | Description                 | Type    |
+===========+=============================+=========+
| name      | The name of the test        | String  |
|           | function                    |         |
+-----------+-----------------------------+---------+
| classname | The name of the ada package | String  |
|           | the test is located in      |         |
+-----------+-----------------------------+---------+
| time      | The time it took to run     | Float   |
|           | this test                   |         |
+-----------+-----------------------------+---------+
| file      | The file path to the test   | String  |
+-----------+-----------------------------+---------+

+----------------+-------------------------------+-------------+
| Child          | Description                   | Cardinality |
+================+===============================+=============+
|                | Only appears when the test    |             |
| `\<failure\>`_ | ended on an assertion failed. |    0..1     |
|                | Describes the test failure.   |             |
+----------------+-------------------------------+-------------+
| <system-out>   | Contains the test's standard  |    0..1     |
|                | output if any.                |             |
+----------------+-------------------------------+-------------+
| <system-err>   | Contains the test's standard  |    0..1     |
|                | error if any.                 |             |
+----------------+-------------------------------+-------------+
|                | Only appears when the test    |             |
| `\<error\>`_   | ended on an unexpected error. |    0..1     |
|                | Describes the test error.     |             |
+----------------+-------------------------------+-------------+

The `<system-out>` element contains only a string body with the 
test stdout, wrapped in a ``<![CDATA[..]]>`` tag to preserve line feed and special characters.

The `<system-err>` element also contains only a string body but with the 
test stderr, once again wrapped in a ``<![CDATA[..]]>`` tag to preserve line feed and special characters.

<failure> 
---------

This element describe a test failure.

It contains a body where the assertion's message, file name and line can be 
found, wrapped in a ``<![CDATA[..]]>`` tag to preserve line feed and special characters.

<error> 
-------

This element describe an unexpected test error.
It contains a body where the error's name, message and traceback can be found, wrapped into a
``<![CDATA[..]]>`` tag to preserve line feed and special characters.

You can find below an example of this format based on the four test cases above.

.. code-block:: xml

      <?xml version="1.0" encoding="utf-8"?>
      <testsuites skipped="0" tests="4" failures="2" errors="1" time="0.000981130">
          <testsuite name="Foo" skipped="0" tests="2" failures="1" errors="1" time="0.000000082">
              <testcase name="Test_Not_Equal" classname="Foo" file="/home/..../foo.ads" time="0.000000063">
                  <failure>
                      <![CDATA[
                        Assertion failed: "Test not implemented." at foo.adb:4
                      ]]>
                  </failure>
              </testcase>
              <testcase name="Test_Sub_Zero_Natural" classname="Foo" file="/home/..../foo.ads" time="0.000000019">
                   <error>
                      <![CDATA[
                      CONSTRAINT_ERROR: foo.adb:11 range check failed
                         [Traceback...] 
                      ]]>
                   </error>              
                   <system-out><![CDATA[This test will crash]]></system-out>
                   <system-err><![CDATA[This test will crash but on stderr]]></system-err>
              </testcase>
          </testsuite>
          <testsuite name="Bar" skipped="0" tests="2" failures="1" errors="0" time="0.000979426">
              <testcase name="Test_Always_True" classname="Bar" file="/home/..../bar.ads" time="0.000979394"/>
              <testcase name="Test_Not_Implemented" classname="Bar" time="0.000000032">
                  <failure message="Test not implemented.">
                      <![CDATA[
                        Assertion failed: "Test not implemented." at bar.adb:4
                      ]]>
                  </failure>
              </testcase>
          </testsuite>
      </testsuites>

.. index:: XML output

XML output
==========

Following is the same harness run using XML output. The XML format used
matches the one used by :index:`CppUnit`.

.. index:: UTF-8 character encoding

Note that text set in the `Assert` subprograms or as test case names should
be compatible with utf-8 character encoding, or the XML will not be
correctly formatted.

.. index:: <TestRun> 

<TestRun>
---------

The top level element of the document, used to hold the different tests
result of the session.

+-----------+----------------------------------+---------+
| Attribute | Description                      | Type    |
+===========+==================================+=========+
| elapsed   | The total time to run all        | String  |
|           | the tests in the session         |         |
+-----------+----------------------------------+---------+

+------------------------+--------------------------------+-------------+
| Child                  | Description                    | Cardinality |
+========================+================================+=============+
| `\<Statistics\>`_      | Statistics on the test results |      1      |
|                        | for this sessions              |             |
+------------------------+--------------------------------+-------------+
| `\<SuccessfulTests\>`_ | List all successful tests      |     0..1    |
+------------------------+--------------------------------+-------------+
| `\<FailedTests\>`_     | List all failed tests          |     0..1    |
+------------------------+--------------------------------+-------------+

<Statistics>
------------

List of element that contains the number of tests, tests passed and failed.

+-----------------+--------------------------------------+-------------+
| Child           | Description                          | Cardinality |
+=================+======================================+=============+
| <Tests>         | The total number of test run         |      1      |
|                 | in this test session                 |             |
+-----------------+--------------------------------------+-------------+
| <FailuresTotal> | The total number of failed test      |             |
|                 | (assertion failed or crashed)        |      1      |
|                 | for the whole test session           |             |
+-----------------+--------------------------------------+-------------+
| <Failures>      | The total number of failed           |      1      |
|                 | assertion for the whole test session |             |
+-----------------+--------------------------------------+-------------+
| <Errors>        | The total number of crashed test     |      1      |
|                 | for the whole test session           |             |
+-----------------+--------------------------------------+-------------+

All those elements have no attribute and have a body containing an ``Integer`` for 
the test count.

<SuccessfulTests>
-----------------

List of all the successful tests.

+--------------+--------------------------------------+-------------+
| Child        | Description                          | Cardinality |
+==============+======================================+=============+
| `\<Test\>`_  | Describe the result of a specific    |    1 .. *   |
|              | test                                 |             |
+--------------+--------------------------------------+-------------+

<FailedTests>
-------------

List of all the failed tests, whether from a failed assertions or an 
unexpected crash.

+--------------+--------------------------------------+-------------+
| Child        | Description                          | Cardinality |
+==============+======================================+=============+
| `\<Test\>`_  | Describe the result of a specific    |    1 .. *   |
|              | test                                 |             |
+--------------+--------------------------------------+-------------+

<Test>
------

This element describe a test result.

+-----------------+---------------------------------+---------+
| Attribute       | Description                     | Type    |
+=================+=================================+=========+
| name            | The name of the test function   | String  |
|                 |                                 |         |
+-----------------+---------------------------------+---------+
| tested_function | The name of the tested function | String  |
|                 |                                 |         |
+-----------------+---------------------------------+---------+
| elapsed         | The time to run the test        | String  |
+-----------------+---------------------------------+---------+

+----------------------------------+--------------------------------------+-------------+
| Child                            | Description                          | Cardinality |
+==================================+======================================+=============+
| <FailureType>                    | The type of failure (either an       |    0 .. 1   |
|                                  | assertion or an error)               |             |
+----------------------------------+--------------------------------------+-------------+
| <Message>                        | The error name or assertion message  |    0 .. 1   |
+----------------------------------+--------------------------------------+-------------+
| `\<Location\>`_                  | The location of the failed assertion |    0 .. 1   |
+----------------------------------+--------------------------------------+-------------+
| `\<Exception\>`_                 | The description of the error         |    0 .. 1   |
+----------------------------------+--------------------------------------+-------------+

Here ``<FailureType>`` and ``<Message``> have no attribute and only a body containing
a ``String``.


<Location>
----------

Represent a location (file/line) from which an assertion was failed.

+--------+-------------------------------------+-------------+
| Child  | Description                         | Cardinality |
+========+=====================================+=============+
| <File> | The file from where the assertion   |      1      |
|        | was raised                          |             |
+--------+-------------------------------------+-------------+
| <Line> | The line in the file from where the |      1      |
|        | assertion was raised                |             |
+--------+-------------------------------------+-------------+

Both those elements have no attribute, ``<File>`` contains a body with a 
``String`` and ``<Line>`` contains a body with an ``Integer``.

<Exception> 
-----------

+---------------+--------------------------------------+-------------+
| Child         | Description                          | Cardinality |
+===============+======================================+=============+
| <Message>     | The error name                       |       1     |
+---------------+--------------------------------------+-------------+
| <Information> | The error message                    |    0 .. 1   |
|               |                                      |             |
+---------------+--------------------------------------+-------------+
| <Traceback>   | The error traceback                  |    0 .. 1   |
+---------------+--------------------------------------+-------------+

Once again those three elements have no attribute and have a body with a ``String``.

You can find below an example of this format based on the four test cases above.

.. code-block:: xml

    <?xml version='1.0' encoding='utf-8' ?>
    <TestRun elapsed="0.995880">
        <Statistics>
            <Tests>4</Tests>
            <FailuresTotal>3</FailuresTotal>
            <Failures>2</Failures>
            <Errors>1</Errors>
        </Statistics>
        <SuccessfulTests>
            <Test name="Test_Always_True" tested_function="Bar.Mock1" elapsed="0.994241"/>
        </SuccessfulTests>
        <FailedTests>
            <Test name="Test_Not_Equal" tested_function="Foo.Mock2" elapsed="0.000061">
                <FailureType>Assertion</FailureType>
                <Message>Value not equal</Message>
                <Location>
                    <File>foo.adb</File>
                    <Line>4</Line>
                </Location>
            </Test>
            <Test name="Test_Not_Implemented" tested_function="Bar.Mock3" elapsed="0.000034">
                <FailureType>Assertion</FailureType>
                <Message>Test not implemented.</Message>
                <Location>
                    <File>bar.adb</File>
                    <Line>4</Line>
                </Location>
            </Test>
            <Test name="Test_Sub_Zero_Natural" tested_function="Foo.Mock4" elapsed="0.000019">
                <FailureType>Error</FailureType>
                <Message>CONSTRAINT_ERROR</Message>
                <Exception>
                    <Message>CONSTRAINT_ERROR</Message>
                    <Information>foo.adb:11 range check failed</Information>
                    <Traceback>
                        [...] 
                    </Traceback>
                </Exception>
            </Test>
        </FailedTests>
    </TestRun>

XML output (deprecated)
=======================

Some changes where made to the XML reporter. The format below is the old format, which is very 
close to the new one. 
This format is deprecated and should not used as it will be removed.

.. code-block:: xml

    <?xml version='1.0' encoding='utf-8' ?>
    <TestRun>
      <Statistics>
        <Tests>4</Tests>
        <FailuresTotal>3</FailuresTotal>
        <Failures>2</Failures>
        <Errors>1</Errors>
      </Statistics>
      <SuccessfulTests>
        <Test>
          <Name>Test_Always_True</Name>
        </Test>
      </SuccessfulTests>
      <FailedTests>
        <Test>
          <Name>Test_Not_Equal</Name>
          <FailureType>Assertion</FailureType>
          <Message>Value not equal</Message>
          <Location>
            <File>foo.adb</File>
            <Line>4</Line>
          </Location>
        </Test>
        <Test>
          <Name>Test_Not_Implemented</Name>
          <FailureType>Assertion</FailureType>
          <Message>Test not implemented.</Message>
          <Location>
            <File>bar.adb</File>
            <Line>4</Line>
          </Location>
        </Test>
        <Test>
          <Name>Test_Sub_Zero_Natural</Name>
          <FailureType>Error</FailureType>
          <Message>CONSTRAINT_ERROR</Message>
          <Exception>
          <Message>CONSTRAINT_ERROR</Message>
          <Information>foo.adb:11 range check failed</Information>
          <Traceback>
            [...]
          </Traceback>
          </Exception>
        </Test>
      </FailedTests>
    </TestRun>

