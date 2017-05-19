//{$IFNDEF VER230}
//  {$LEGACYIFEND ON}
{//$ENDIF}

{$IF CompilerVersion >= 23} // Delphi XE2
  {$DEFINE Ver230Up}
  {$IF CompilerVersion >= 24} // Delphi XE3
    {$DEFINE Ver240Up}
    {$IF CompilerVersion >= 25} // Delphi XE4
      {$DEFINE Ver250Up}
      {$IF CompilerVersion >= 26} // Delphi XE5
        {$DEFINE Ver260Up}
        {$IF CompilerVersion >= 27} // Delphi XE6
          {$DEFINE Ver270Up}
          {$IF CompilerVersion >= 28} // Delphi XE7
            {$DEFINE Ver280Up}
            {$IF CompilerVersion >= 29} // Delphi XE8
              {$DEFINE Ver290Up}
			        {$IF CompilerVersion >= 30} // Delphi XE10
 			          {$DEFINE Ver300Up}
  			      {$IFEND}
            {$IFEND}
          {$IFEND}
        {$IFEND}
      {$IFEND}
    {$IFEND}
  {$IFEND}
{$IFEND}

{$IF CompilerVersion <= 30} // Delphi XE10
  {$DEFINE Ver300Down}
  {$IF CompilerVersion <= 29} // Delphi XE8
    {$DEFINE Ver290Down}
    {$IF CompilerVersion <= 28} // Delphi XE7
      {$DEFINE Ver280Down}
      {$IF CompilerVersion <= 27} // Delphi XE6
        {$DEFINE Ver270Down}
        {$IF CompilerVersion <= 26} // Delphi XE5
          {$DEFINE Ver260Down}
          {$IF CompilerVersion <= 25} // Delphi XE4
            {$DEFINE Ver250Down}
            {$IF CompilerVersion <= 24} // Delphi XE3
              {$DEFINE Ver240Down}
              {$IF CompilerVersion <= 23} // Delphi XE2
                {$DEFINE Ver230Down}
              {$IFEND}
			      {$IFEND}
          {$IFEND}
        {$IFEND}
      {$IFEND}
    {$IFEND}
  {$IFEND}
{$IFEND}

// I've turned the warnings for depricated symbols of because this unit must
// compile in Delphi Xe2 to Xe10 and I don't want too many IFDEF's

{$WARN SYMBOL_DEPRECATED OFF}


//{$DEFINE SVGDEBUG}
//{$DEFINE Profiler}


