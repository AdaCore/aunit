with System;
function Dummy_SS_Get return System.Address;
pragma Export (C, Dummy_SS_Get, "__gnat_get_secondary_stack");
