////////////////////////////////////////////////////////////////////////////////
//
//  FileName    :   UMainForm.pas
//  Creator     :   Shen Min
//  Date        :   2021-10-03
//  Comment     :
//
//  Copyright (c) 2021 Shen Min (https://nicrosoft.net)
//
////////////////////////////////////////////////////////////////////////////////

unit UMainForm;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

    { TMainForm }

    TMainForm = class(TForm)
        Button1 : TButton;
        Button2 : TButton;
        Button3 : TButton;
        Button4 : TButton;
        Button5 : TButton;
        Button6 : TButton;
        Button7 : TButton;
        Button8 : TButton;
        GroupBox1 : TGroupBox;
        GroupBox2 : TGroupBox;
        procedure Button8Click(Sender : TObject);
        procedure Button1Click(Sender : TObject);
        procedure Button2Click(Sender : TObject);
        procedure Button3Click(Sender : TObject);
        procedure Button4Click(Sender : TObject);
        procedure Button5Click(Sender : TObject);
        procedure Button6Click(Sender : TObject);
        procedure Button7Click(Sender : TObject);
    private
        procedure do_show_msg();

        procedure delay_method_in_thread();
        procedure delay_proc_in_thread();

    public

    end;

var
    MainForm : TMainForm;

implementation

uses UDelayRun;

{$R *.lfm}

procedure do_show_msg_proc();
begin
    ShowMessage('Hello from Procedure.');
end;

{ TMainForm }

procedure TMainForm.Button1Click(Sender : TObject);
begin
    shared_delay().run(@do_show_msg, 1000);
end;

procedure TMainForm.Button2Click(Sender : TObject);
begin
    shared_delay().run(@do_show_msg, 2000);
end;

procedure TMainForm.Button3Click(Sender : TObject);
begin
    shared_delay().cancel(@do_show_msg);
end;

procedure TMainForm.Button4Click(Sender : TObject);
begin
    shared_delay().run(@do_show_msg_proc, 1000);
end;

procedure TMainForm.Button5Click(Sender : TObject);
begin
    shared_delay().run(@do_show_msg_proc, 2000);
end;

procedure TMainForm.Button6Click(Sender : TObject);
begin
    shared_delay().cancel(@do_show_msg_proc);
end;

procedure TMainForm.Button7Click(Sender : TObject);
begin
    TThread.ExecuteInThread(@delay_method_in_thread);
end;

procedure TMainForm.Button8Click(Sender : TObject);
begin
    TThread.ExecuteInThread(@delay_proc_in_thread);
end;

procedure TMainForm.do_show_msg();
begin
    ShowMessage('Hello from Method.');
end;

procedure TMainForm.delay_method_in_thread();
begin
    shared_delay().cancel();
    shared_delay().run(@do_show_msg, 1000);
end;

procedure TMainForm.delay_proc_in_thread();
begin
    shared_delay().cancel();
    shared_delay().run(@do_show_msg_proc, 1000);
end;

end.

