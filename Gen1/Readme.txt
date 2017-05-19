First generation of the open source editor for the G2 is a Windows only application. It also includes a VST which I made using Tobybear's VST SDK for Delphi. It can be used with Windows user interface reader (JAWS) for visually impaired users.

Project files are Delphi version specific. There might be differences in Delphi IDE and sourcecode library between Delphi versions, so project files (application and packages) are made for each Delphi version specifically and the code units are shared, possibly with IFDEF's with version numbers to make them compile in each Delphi version.

supported versions:
XE2 - project files for Delphi XE2

Build and install package FPCXMLPackage<version> in Source\Third_party_code\FPC_XML
Build and install package MidiComponents<version> in Source\Third_party_code\MIDIIO
Build and install package NMG2_controls_<version> in <version>\Package
Build and install package G2VCLPackage_<version> in <version>\Package\Vcl

After this you can open and compile the Editor application project in <version>\Editor

TODO: there are a lot of compiler warnings and hints, these should really be resolved.