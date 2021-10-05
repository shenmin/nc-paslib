////////////////////////////////////////////////////////////////////////////////
//
//  FileName    :   delay_run.lpr
//  Creator     :   Shen Min
//  Date        :   2021-10-03
//  Comment     :
//
//  Copyright (c) 2021 Shen Min (https://nicrosoft.net)
//
////////////////////////////////////////////////////////////////////////////////

program delay_run;

{$mode objfpc}{$H+}

uses
    {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}{$ENDIF}
    Interfaces, // this includes the LCL widgetset
    Forms, UMainForm, UDelayRun, UThreads
    { you can add units after this };

{$R *.res}

begin
    RequireDerivedFormResource := True;
    Application.Scaled := True;
    Application.Initialize;
    Application.CreateForm(TMainForm, MainForm);
    Application.Run;
end.

