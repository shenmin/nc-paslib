////////////////////////////////////////////////////////////////////////////////
//
//  FileName    :   animate.lpr
//  Creator     :   Shen Min
//  Date        :   2024-02-06
//  Comment     :
//
//  Copyright (c) 2024 Shen Min (https://nicrosoft.net)
//
////////////////////////////////////////////////////////////////////////////////

program animate;

{$mode objfpc}{$H+}

uses
    {$IFDEF UNIX}
    cthreads,
    {$ENDIF}
    {$IFDEF HASAMIGA}
    athreads,
    {$ENDIF}
    Interfaces, // this includes the LCL widgetset
    Forms, UMainForm, UAnimate
    { you can add units after this };

{$R *.res}

begin
    RequireDerivedFormResource:=True;
    Application.Scaled:=True;
    Application.Initialize;
    Application.CreateForm(TForm1, Form1);
    Application.Run;
end.

