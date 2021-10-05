////////////////////////////////////////////////////////////////////////////////
//
//  FileName    :   delay_run.dpr
//  Creator     :   Shen Min
//  Date        :   2021-10-05
//  Comment     :
//
//  Copyright (c) 2021 Shen Min (https://nicrosoft.net)
//
////////////////////////////////////////////////////////////////////////////////

program delay_run;

uses
  Vcl.Forms,
  UMainForm in 'UMainForm.pas' {MainForm},
  UDelayRun in '..\..\..\src\UDelayRun.pas',
  UThreads in '..\..\..\src\UThreads.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
