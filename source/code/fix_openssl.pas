// SPDX-License-Identifier: MIT
unit fix_openssl;

{$mode ObjFPC}{$H+}

interface

uses
  opensslsockets, OpenSSL;

implementation
initialization
  // FPC 3.2.2 is missing support for the latest openSSL 3, will be fix in the future release.
  // Latest openssl.pas https://gitlab.com/freepascal.org/fpc/source/-/blob/main/packages/openssl/src/openssl.pas?ref_type=heads
  // Copy this newer SSL detection into the older openssl code used by the present FPC 3.2.2
{$IFDEF VER3_2}
{$IFDEF WINDOWS}
  DLLSSLName3 := {$IFDEF WIN64}'libssl-3-x64.dll'{$ELSE}'libssl-3.dll'{$ENDIF};
  DLLUtilName2 := {$IFDEF WIN64}'libcrypto-3-x64.dll'{$ELSE}'libcrypto-3.dll'{$ENDIF};
{$ELSE WINDOWS}
{$IFDEF DARWIN}
  if High(OpenSSL.DLLVersions) >= 19 then
  begin
    // macOS version
    // LibreSSL
    OpenSSL.DLLVersions[1] := '.48';
    OpenSSL.DLLVersions[2] := '.47';
    OpenSSL.DLLVersions[3] := '.46';
    OpenSSL.DLLVersions[4] := '.45';
    OpenSSL.DLLVersions[5] := '.44';
    OpenSSL.DLLVersions[6] := '.43';
    OpenSSL.DLLVersions[7] := '.35';

    // OpenSSL
    OpenSSL.DLLVersions[8] := '.3';
    OpenSSL.DLLVersions[9] := '.1.1';
    OpenSSL.DLLVersions[10] := '.11';
    OpenSSL.DLLVersions[11] := '.10';
    OpenSSL.DLLVersions[12] := '.1.0.6';
    OpenSSL.DLLVersions[13] := '.1.0.5';
    OpenSSL.DLLVersions[14] := '.1.0.4';
    OpenSSL.DLLVersions[15] := '.1.0.3';
    OpenSSL.DLLVersions[16] := '.1.0.2';
    OpenSSL.DLLVersions[17] := '.1.0.1';
    OpenSSL.DLLVersions[18] := '.1.0.0';
    OpenSSL.DLLVersions[19] := '.0.9.8';
  end;
{$ElSE DARWIN}
  // Unix/Linux version of FPC need openSSL 3 in the detection list
  OpenSSL.DLLVersions[Length(OpenSSL.DLLVersions) - 1] := '.3';
{$ENDIF DARWIN}
{$ENDIF WINDOWS}
{$ENDIF VER3_2}
end.

