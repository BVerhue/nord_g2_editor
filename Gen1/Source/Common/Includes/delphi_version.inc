{$IFDEF CONDITIONALEXPRESSIONS}
  {$IFDEF BCB}
    {$DEFINE G2_BCB}
  {$ELSE}
    {$DEFINE G2_Delphi}
  {$ENDIF}

  {$IF CompilerVersion >= 14}
    {$DEFINE G2_VER140_up} // Delphi 6
    {$IFDEF LINUX}
      {$DEFINE G2_KYLIX}
      {$IF RTLVersion = 14.2}
        {$DEFINE G2_KYLIX2} // Kylix 2
      {$IFEND}
      {$IF RTLVersion = 14.5}
        {$DEFINE G2_KYLIX3} // Kylix 3
      {$IFEND}
    {$ENDIF}
    {$IF CompilerVersion >= 15}
      {$DEFINE G2_VER150_up}  // Delphi 7
      {$IF CompilerVersion >= 16}
        {$DEFINE G2_VER160_up}  // Delphi 8
        {$IF CompilerVersion >= 17}
          {$DEFINE G2_VER170_up}  // Delphi 9/2005
          {$IF CompilerVersion >= 18}
            {$DEFINE G2_VER180_up}  // BDS 2006
            {$IFDEF VER185}
              {$DEFINE G2_VER185_up}  // Delphi 2007
            {$ENDIF}
            {$IF CompilerVersion >= 19}
              {$DEFINE G2_VER190_up}  // Delphi .NET 2007
              {$IF CompilerVersion >= 20}
                {$DEFINE G2_VER200_up}  // RAD Studio 2009      
                {$IF Compilerversion >= 21}
                  {$DEFINE G2_VER210_up} // Rad Studio 2010
                  {$IF Compilerversion >= 22}
                    {$DEFINE G2_VER220_up} // Delphi XE
                    {$IF Compilerversion >= 23}
                      {$DEFINE G2_VER230_up} // Delphi XE2
                      {$IF Compilerversion >= 24}
                        {$DEFINE G2_VER240_up} // Delphi XE3
                      {$IFEND}
                    {$IFEND}
                  {$IFEND}
                {$IFEND}
              {$IFEND}
            {$IFEND}
          {$IFEND}
        {$IFEND}
      {$IFEND}
    {$IFEND}
  {$IFEND}
{$ELSE not CONDITIONALEXPRESSIONS}
  Sorry, but this version does not support the IDE
  you are using.
{$ENDIF}