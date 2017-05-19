//******************************************************************************
//
//  DVstFxStore.pas
//  21 May 2003
//
//  Part of the VST 2.3 SDK for Delphi
//  by Frederic Vanmol
//     http://www.axiworld.be
//     frederic@axiworld.be
//
//------------------------------------------------------------------------------
//
// VST Plug-Ins SDK
// Version 2.3 extension
// © 2003, Steinberg Media Technologies, All Rights Reserved
//
// Different structures for persistent data for VST Plugins:
//      - Preset (or Program) without Chunk (->fxPreset)
//      - Preset (or Program) with Chunk (->fxChunkPreset)
//      - Bank without Chunk (->fxBank)
//      - Bank with Chunk (->fxChunkBank)
//
//******************************************************************************
unit DVstFxStore;

interface

const
     cMagic           = 'CcnK';

     presetMagic      = 'FxCk';
     bankMagic        = 'FxBk';

     chunkPresetMagic = 'FPCh';
     chunkBankMagic   = 'FBCh';

     // for old compatibility (renaming)
     chunkGlobalMagic = 'FxCh'; // not used
     fMagic           = presetMagic;

type
    //--------------------------------------------------------------------
    // For Preset (Program) (.fxp) without chunk (magic = 'FxCk')
    //--------------------------------------------------------------------
    fxPreset = packed record
      chunkMagic : longint;   // 'CcnK'
      byteSize   : longint;   // of this chunk, excl. magic + byteSize

      fxMagic    : longint;   // 'FxCk'
      version    : longint;
      fxID       : longint;   // fx unique id
      fxVersion  : longint;

      numParams  : longint;
      prgName    : array[0..27] of AnsiChar;
      params     : pointer; //array[0..0] of single;    // variable no. of parameters
    end;

    //--------------------------------------------------------------------
    // For Preset (Program) (.fxp) with chunk (magic = 'FPCh')
    //--------------------------------------------------------------------    
    fxChunkSet = packed record
      chunkMagic  : longint;                // 'CcnK'
      byteSize    : longint;                // of this chunk, excl. magic + byteSize

      fxMagic     : longint;                // 'FPCh'
      version     : longint;
      fxID        : longint;                // fx unique id
      fxVersion   : longint;

      numPrograms : longint;
      prgName     : array[0..27] of AnsiChar;

      chunkSize   : longint;
      chunk       : pointer; //array[0..7] of char;    // variable
    end;

    //--------------------------------------------------------------------
    // For Bank (.fxb) without chunk (magic = 'FxBk')
    //--------------------------------------------------------------------
    fxSet = packed record
      chunkMagic  : longint;                   // 'CcnK'
      byteSize    : longint;                   // of this chunk, excl. magic + byteSize

      fxMagic     : longint;                   // 'FxBk'
      version     : longint;
      fxID        : longint;                   // fx unique id
      fxVersion   : longint;

      numPrograms : longint;
      future      : array[0..127] of byte;

      programs    : pointer;//array[0..0] of fxPreset;  // variable no. of programs
    end;


    //--------------------------------------------------------------------
    // For Bank (.fxb) with chunk (magic = 'FBCh')
    //--------------------------------------------------------------------
    fxChunkBank = packed record
      chunkMagic  : longint;                // 'CcnK'
      byteSize    : longint;                // of this chunk, excl. magic + byteSize

      fxMagic     : longint;                // 'FBCh'
      version     : longint;
      fxID        : longint;                // fx unique id
      fxVersion   : longint;

      numPrograms : longint;
      future      : array[0..127] of byte;

      chunkSize   : longint;
      chunk       : pointer; //array[0..7] of char;    // variable
    end;



implementation

end.
