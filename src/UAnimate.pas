////////////////////////////////////////////////////////////////////////////////
//
//  FileName    :   UAnimate.pas
//  Creator     :   Shen Min
//  Date        :   2024-02-06
//  Comment     :
//
//  Copyright (c) 2024 Shen Min (https://nicrosoft.net)
//
////////////////////////////////////////////////////////////////////////////////

unit UAnimate;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

interface

uses {$IFNDEF FPC}Windows, Types,{$ENDIF} Classes, SysUtils, Controls, ExtCtrls, Forms;

type
    TncAnimateEvent = procedure (const Sender : TObject; const animate_tag : Integer) of object;
    TncAnimateProgressEvent = procedure (const Sender : TObject; const animate_tag : Integer; var progress : real) of object;

    TncAnimateRec = record
        x, y, w, h : Integer;
        d : Cardinal;
        t : Integer;
        k : Boolean;
    end;
    PTncAnimateRec = ^TncAnimateRec;

    { TncAnimate }

    TncAnimate = class
    private
        m_control : TControl;
        m_timer : TTimer;
        m_on_animate_complete : TncAnimateEvent;
        m_on_animate_progress : TncAnimateProgressEvent;
        m_cur_tag : Integer;
        m_start_tc : Int64;
        m_duration : Cardinal;

        m_start_x : Integer;
        m_start_y : Integer;
        m_start_w : Integer;
        m_start_h : Integer;
        m_start_a : Integer;
        m_target_x : Integer;
        m_target_y : Integer;
        m_target_w : Integer;
        m_target_h : Integer;
        m_target_a : Integer;

        procedure on_timer(Sender : TObject);

    public
        constructor Create(const control : TControl); reintroduce;
        destructor Destroy(); override;

        procedure move_to(const x, y : Integer; const duration : Cardinal; const tag : Integer);
        procedure size_to(const w, h : Integer; const keep_center : Boolean; const duration : Cardinal; const tag : Integer);
        procedure expand_to_left(const w : Integer; const duration : Cardinal; const tag : Integer);
        procedure change_to(const x, y, w, h : Integer; const duration : Cardinal; const tag : Integer);
        procedure alpha_to(const alpha : Integer; const duration : Cardinal; const tag : Integer);

        procedure start();

        property OnAnimateComplete : TncAnimateEvent read m_on_animate_complete write m_on_animate_complete;
        property OnAnimateProgress : TncAnimateProgressEvent read m_on_animate_progress write m_on_animate_progress;

    end;

    { TncAnimateGroup }

    TncAnimateGroup = class
    private
        m_animate_list : TList;
        m_animate : TncAnimate;
        m_running : Boolean;

        procedure run_animate(const ani : PTncAnimateRec; id : Integer);
        procedure on_ani_complete(const Sender : TObject; const tag : Integer);

    public
        constructor Create(const control : TControl); reintroduce;
        destructor Destroy(); override;

        procedure add_size_animate(const w, h : Integer; const keep_center : Boolean; const duration : Cardinal);
        procedure add_expand_to_left_animate(const w : Integer; const duration : Cardinal);
        procedure add_move_animate(const x, y : Integer; const duration : Cardinal);
        procedure add_change_animate(const x, y, w, h : Integer; const duration : Cardinal);

        procedure run(); overload;
        procedure run(const progress_callback : TncAnimateProgressEvent); overload;

    end;

    { TncAnimateAsyncGroup }

    TncAnimateAsyncGroup = class
    private
        m_animate_list : TList;
        m_running : Integer;

        procedure on_ani_complete(const Sender : TObject; const tag : Integer);

    public
        constructor Create();
        destructor Destroy(); override;

        procedure add_size_animate(const control : TControl; const w, h : Integer; const keep_center : Boolean; const duration : Cardinal);
        procedure add_expand_to_left_animate(const control : TControl; const w : Integer; const duration : Cardinal);
        procedure add_move_animate(const control : TControl; const x, y : Integer; const duration : Cardinal);
        procedure add_change_animate(const control : TControl; const x, y, w, h : Integer; const duration : Cardinal);
        procedure add_alpha_animate(const form : TForm; const a : Integer; const duration : Cardinal);

        procedure run(); overload;
        procedure run(const progress_callback : TncAnimateProgressEvent); overload;

    end;


implementation

{ TncAnimate }

procedure TncAnimate.on_timer(Sender: TObject);
var
    tc_diff : Int64;
    ratio : real;
begin
    if not m_timer.Enabled then
    begin
        Exit;
    end;

    tc_diff := GetTickCount64() - m_start_tc;
    if tc_diff >= m_duration then
    begin
        if (m_target_a >= 0) and (m_start_a >= 0) then
        begin
            (m_control as TForm).AlphaBlendValue := m_target_a;
        end
        else
        begin
            m_control.Left := m_target_x;
            m_control.Top := m_target_y;
            m_control.Width := m_target_w;
            m_control.Height := m_target_h;

            m_control.Invalidate;
        end;

        m_timer.Enabled := false;
        if Assigned(m_on_animate_complete) then
        begin
            m_on_animate_complete(self, m_cur_tag);
        end;
    end
    else
    begin
        ratio := tc_diff / m_duration;

        if Assigned(m_on_animate_progress) then
        begin
            m_on_animate_progress(self, m_cur_tag, ratio);
        end;

        if (m_target_a >= 0) and (m_start_a >= 0) then
        begin
            (m_control as TForm).AlphaBlendValue := m_start_a + Trunc((m_target_a - m_start_a) * ratio);
        end
        else
        begin
            m_control.Left := m_start_x + Round((m_target_x - m_start_x) * ratio);
            m_control.Top := m_start_y + Round((m_target_y - m_start_y) * ratio);
            m_control.Width := m_start_w + Round((m_target_w - m_start_w) * ratio);
            m_control.Height := m_start_h + Round((m_target_h - m_start_h) * ratio);

            m_control.Invalidate;
        end;
    end;
end;

constructor TncAnimate.Create(const control: TControl);
begin
    inherited Create();

    m_control := control;
    m_timer := TTimer.Create(nil);
    m_timer.Interval := 5;
    m_timer.Enabled := false;
    m_timer.OnTimer := on_timer;
end;

destructor TncAnimate.Destroy;
begin
    m_timer.Free;
    m_timer := nil;

    m_control := nil;

    inherited Destroy();
end;

procedure TncAnimate.move_to(const x, y: Integer; const duration: Cardinal; const tag: Integer);
begin
    change_to(x, y, m_control.Width, m_control.Height, duration, tag);
end;

procedure TncAnimate.size_to(const w, h: Integer; const keep_center: Boolean; const duration: Cardinal; const tag: Integer);
var
    diff_w, diff_h : Integer;
begin
    if keep_center then
    begin
        diff_w := w - m_control.Width;
        diff_h := h - m_control.Height;
        change_to(m_control.Left - diff_w div 2, m_control.Top - diff_h div 2, w, h, duration, tag);
    end
    else
    begin
        change_to(m_control.Left, m_control.Top, w, h, duration, tag);
    end;
end;

procedure TncAnimate.expand_to_left(const w: Integer; const duration: Cardinal; const tag: Integer);
var
    diff_w : Integer;
begin
    diff_w := w - m_control.Width;
    change_to(m_control.Left - diff_w, m_control.Top, w, m_control.Height, duration, tag);
end;

procedure TncAnimate.change_to(const x, y, w, h: Integer; const duration: Cardinal; const tag: Integer);
begin
    m_cur_tag := tag;

    m_start_x := m_control.Left;
    m_start_y := m_control.Top;
    m_start_w := m_control.Width;
    m_start_h := m_control.Height;
    m_start_a := -1;

    m_target_x := x;
    m_target_y := y;
    m_target_w := w;
    m_target_h := h;
    m_target_a := -1;
    m_duration := duration;

    m_start_tc := GetTickCount64();
end;

procedure TncAnimate.alpha_to(const alpha: Integer; const duration: Cardinal; const tag: Integer);
var
    f : TForm;
begin
    if m_control is TForm then
    begin
        m_cur_tag := tag;

        f := m_control as TForm;

        f.AlphaBlend := true;
        m_start_a := f.AlphaBlendValue;
        m_target_a := alpha;
        m_duration := duration;

        m_start_tc := GetTickCount64();
    end;
end;

procedure TncAnimate.start();
begin
    m_timer.Enabled := true;
end;

{ TncAnimateGroup }

procedure TncAnimateGroup.run_animate(const ani: PTncAnimateRec; id: Integer);
begin
    if ani.t = 0 then
    begin // change
        m_animate.change_to(ani.x, ani.y, ani.w, ani.h, ani.d, id);
        m_animate.start();
    end
    else if ani.t = 1 then
    begin // move
        m_animate.move_to(ani.x, ani.y, ani.d, id);
        m_animate.start();
    end
    else if ani.t = 2 then
    begin // size
        m_animate.size_to(ani.w, ani.h, ani.k, ani.d, id);
        m_animate.start();
    end
    else if ani.t = 3 then
    begin // expand to left
        m_animate.expand_to_left(ani.w, ani.d, id);
        m_animate.start();
    end
    else
    begin
        on_ani_complete(self, id);
    end;
end;

procedure TncAnimateGroup.on_ani_complete(const Sender: TObject; const tag: Integer);
var
    ani : PTncAnimateRec;
begin
    if tag < m_animate_list.Count - 1 then
    begin
        ani := m_animate_list[tag + 1];
        run_animate(ani, tag + 1);
    end
    else
    begin
        m_running := false;
    end;
end;

constructor TncAnimateGroup.Create(const control: TControl);
begin
    inherited Create();

    m_animate_list := TList.Create;
    m_animate := TncAnimate.Create(control);
    m_animate.OnAnimateComplete := on_ani_complete;
end;

destructor TncAnimateGroup.Destroy;
var
    i : Integer;
    ani : PTncAnimateRec;
begin
    m_animate.Free;
    m_animate := nil;

    for i := 0 to m_animate_list.Count - 1 do
    begin
        ani := m_animate_list[i];
        Dispose(ani);
    end;
    m_animate_list.Free;
    m_animate_list := nil;

    inherited Destroy();
end;

procedure TncAnimateGroup.add_size_animate(const w, h: Integer; const keep_center: Boolean; const duration: Cardinal);
var
    ani : PTncAnimateRec;
begin
    New(ani);
    ani.x := 0;
    ani.y := 0;
    ani.w := w;
    ani.h := h;
    ani.t := 2;
    ani.k := keep_center;
    ani.d := duration;

    m_animate_list.Add(ani);
end;

procedure TncAnimateGroup.add_expand_to_left_animate(const w: Integer; const duration: Cardinal);
var
    ani : PTncAnimateRec;
begin
    New(ani);
    ani.x := 0;
    ani.y := 0;
    ani.w := w;
    ani.h := 0;
    ani.t := 3;
    ani.k := false;
    ani.d := duration;

    m_animate_list.Add(ani);
end;

procedure TncAnimateGroup.add_move_animate(const x, y: Integer; const duration: Cardinal);
var
    ani : PTncAnimateRec;
begin
    New(ani);
    ani.x := x;
    ani.y := y;
    ani.w := 0;
    ani.h := 0;
    ani.t := 1;
    ani.k := false;
    ani.d := duration;

    m_animate_list.Add(ani);
end;

procedure TncAnimateGroup.add_change_animate(const x, y, w, h: Integer; const duration: Cardinal);
var
    ani : PTncAnimateRec;
begin
    New(ani);
    ani.x := x;
    ani.y := y;
    ani.w := w;
    ani.h := h;
    ani.t := 0;
    ani.k := false;
    ani.d := duration;

    m_animate_list.Add(ani);
end;

procedure TncAnimateGroup.run();
begin
    run(nil);
end;

procedure TncAnimateGroup.run(const progress_callback: TncAnimateProgressEvent);
var
    ani : PTncAnimateRec;
begin
    m_animate.OnAnimateProgress := progress_callback;

    m_running := false;
    if m_animate_list.Count > 0 then
    begin
        m_running := true;
        ani := m_animate_list[0];
        run_animate(ani, 0);
    end;

    while m_running do
    begin
        Application.ProcessMessages;
    end;
end;

{ TncAnimateAsyncGroup }

procedure TncAnimateAsyncGroup.on_ani_complete(const Sender: TObject; const tag: Integer);
begin
    Dec(m_running);
end;

constructor TncAnimateAsyncGroup.Create();
begin
    inherited Create();

    m_animate_list := TList.Create;
end;

destructor TncAnimateAsyncGroup.Destroy;
var
    i : Integer;
    ani : TncAnimate;
begin
    for i := 0 to m_animate_list.Count - 1 do
    begin
        ani := m_animate_list[i];
        ani.Free;
    end;
    m_animate_list.Free;
    m_animate_list := nil;

    inherited Destroy();
end;

procedure TncAnimateAsyncGroup.add_size_animate(const control: TControl; const w, h: Integer; const keep_center: Boolean; const duration: Cardinal);
var
    ani : TncAnimate;
begin
    ani := TncAnimate.Create(control);
    ani.size_to(w, h, keep_center, duration, m_animate_list.Count);
    m_animate_list.Add(ani);
end;

procedure TncAnimateAsyncGroup.add_expand_to_left_animate(const control: TControl; const w: Integer; const duration: Cardinal);
var
    ani : TncAnimate;
begin
    ani := TncAnimate.Create(control);
    ani.expand_to_left(w, duration, m_animate_list.Count);
    m_animate_list.Add(ani);
end;

procedure TncAnimateAsyncGroup.add_move_animate(const control: TControl; const x, y: Integer; const duration: Cardinal);
var
    ani : TncAnimate;
begin
    ani := TncAnimate.Create(control);
    ani.move_to(x, y, duration, m_animate_list.Count);
    m_animate_list.Add(ani);
end;

procedure TncAnimateAsyncGroup.add_change_animate(const control: TControl; const x, y, w, h: Integer; const duration: Cardinal);
var
    ani : TncAnimate;
begin
    ani := TncAnimate.Create(control);
    ani.change_to(x, y, w, h, duration, m_animate_list.Count);
    m_animate_list.Add(ani);
end;

procedure TncAnimateAsyncGroup.add_alpha_animate(const form: TForm; const a: Integer; const duration: Cardinal);
var
    ani : TncAnimate;
begin
    ani := TncAnimate.Create(form);
    ani.alpha_to(a, duration, m_animate_list.Count);
    m_animate_list.Add(ani);
end;

procedure TncAnimateAsyncGroup.run();
begin
    run(nil);
end;

procedure TncAnimateAsyncGroup.run(const progress_callback: TncAnimateProgressEvent);
var
    i : Integer;
    ani : TncAnimate;
begin
    m_running := m_animate_list.Count;

    for i := 0 to m_animate_list.Count - 1 do
    begin
        ani := m_animate_list[i];
        ani.OnAnimateProgress := progress_callback;
        ani.OnAnimateComplete := on_ani_complete;
        ani.start();
    end;

    while m_running > 0 do
    begin
        Application.ProcessMessages;
    end;
end;

end.

