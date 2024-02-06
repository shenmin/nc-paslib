////////////////////////////////////////////////////////////////////////////////
//
//  FileName    :   UMainForm.pas
//  Creator     :   Shen Min
//  Date        :   2024-02-06
//  Comment     :
//
//  Copyright (c) 2024 Shen Min (https://nicrosoft.net)
//
////////////////////////////////////////////////////////////////////////////////

unit UMainForm;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Math,
    ComCtrls;

type

    { TForm1 }

    TForm1 = class(TForm)
        Button1: TButton;
        Button2: TButton;
        Button3: TButton;
        GroupBox1: TGroupBox;
        Image1: TImage;
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Panel1: TPanel;
        procedure Button1Click(Sender: TObject);
        procedure Button2Click(Sender: TObject);
        procedure Button3Click(Sender: TObject);
    private
        procedure acceleration_animation(const Sender : TObject; const animate_tag : Integer; var progress : real);
        procedure deceleration_animation(const Sender : TObject; const animate_tag : Integer; var progress : real);
    public

    end;

var
    Form1: TForm1;

implementation

uses UAnimate;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
    animate_group : TncAnimateGroup;
begin
    Button1.Enabled := false;
    animate_group := TncAnimateGroup.Create(Panel1);
    animate_group.add_size_animate(Button1.Left, Panel1.Height, false, 500); // Expand the Panel1 to Button1's left border within 500ms.
    animate_group.add_size_animate(100, Panel1.Height, false, 500); // Animate the width of Panel1 to 100 within 500ms.
    animate_group.run();
    animate_group.Free();
    Button1.Enabled := true;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
    animate_group : TncAnimateAsyncGroup;
begin
    animate_group := TncAnimateAsyncGroup.Create();
    animate_group.add_move_animate(Label2, GroupBox1.Width - Label2.Width - 10, Label2.Top, 1000);
    animate_group.add_change_animate(Image1, GroupBox1.Width - 144 - 10, Image1.Top, 144, 144, 1000);
    animate_group.run(@acceleration_animation);
    animate_group.Free();

    animate_group := TncAnimateAsyncGroup.Create();
    animate_group.add_move_animate(Label2, 32, Label2.Top, 1000);
    animate_group.add_change_animate(Image1, 56, Image1.Top, 72, 72, 1000);
    animate_group.run(@deceleration_animation);
    animate_group.Free();
end;

procedure TForm1.Button3Click(Sender: TObject);
var
    animate_group : TncAnimateAsyncGroup;
begin
    animate_group := TncAnimateAsyncGroup.Create();
    animate_group.add_alpha_animate(self, 0, 1000);
    animate_group.run();
    animate_group.Free();

    animate_group := TncAnimateAsyncGroup.Create();
    animate_group.add_alpha_animate(self, 255, 1000);
    animate_group.run();
    animate_group.Free();
end;

procedure TForm1.acceleration_animation(const Sender: TObject; const animate_tag: Integer; var progress: real);
begin
    progress := power(progress, 3); // make an acceleration effect
end;

procedure TForm1.deceleration_animation(const Sender: TObject; const animate_tag: Integer; var progress: real);
begin
    progress := power(progress, 1/3); // make a deceleration effect
end;

end.

