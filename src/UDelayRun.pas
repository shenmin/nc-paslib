////////////////////////////////////////////////////////////////////////////////
//
//  FileName    :   UDelayRun.pas
//  Creator     :   Shen Min
//  Date        :   2021-10-03
//  Comment     :
//
//  Copyright (c) 2021 Shen Min (https://nicrosoft.net)
//
////////////////////////////////////////////////////////////////////////////////

unit UDelayRun;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF}

interface

uses {$IFNDEF FPC}Windows, Types,{$ENDIF} Classes, SysUtils, ExtCtrls,
     UThreads;

type
    TncDelayRec = record
        run_time : Int64;
        proc : TProcedure;
        proc_method : TThreadMethod;
        cancelled : Boolean;
    end;
    PTncDelayRec = ^TncDelayRec;

    { TncDelay }

    TncDelay = class
    private
        m_cs : TRTLCriticalSection;
        m_runlist : TList;
        m_timer : TTimer;
        m_processing_cancelling : Boolean;
        m_running : Boolean;
        m_disabled : Boolean;

        procedure enable_timer();
        procedure disable_timer();

        procedure on_timer(Sender : TObject);

        procedure do_proc(const proc : TProcedure);
        procedure do_proc_method(const proc_method : TThreadMethod);

    public
        constructor Create();
        destructor Destroy(); override;

        procedure run(const proc : TProcedure; const after_ms : UInt64); overload;
        procedure run(const proc_method : TThreadMethod; const after_ms : UInt64); overload;

        procedure cancel(); overload;
        procedure cancel(const proc : TProcedure); overload;
        procedure cancel(const proc_method : TThreadMethod); overload;

        procedure begin_disable();
        procedure end_disable();

    end;

    function shared_delay() : TncDelay;

implementation

var
    l_obj : TncDelay = nil;

function shared_delay() : TncDelay;
begin
    if not Assigned(l_obj) then
    begin
        l_obj := TncDelay.Create();
    end;

    Result := l_obj;
end;

procedure init_delay();
begin
    shared_delay();
end;

procedure uninit_delay();
begin
    if l_obj <> nil then
    begin
        l_obj.Free();
        l_obj := nil;
    end;
end;

{ TncDelay }

procedure TncDelay.cancel();
var
    i : Integer;
    item : PTncDelayRec;
begin
    EnterCriticalSection(m_cs);

    m_processing_cancelling := true;

    for i := 0 to m_runlist.Count - 1 do
    begin
        item := m_runlist[i];
        Dispose(item);
    end;

    m_runlist.Clear();

    m_processing_cancelling := false;

    TncThread.ensure_in_main_thread(disable_timer);

    LeaveCriticalSection(m_cs);
end;

procedure TncDelay.begin_disable();
begin
    EnterCriticalSection(m_cs);

    m_disabled := true;

    LeaveCriticalSection(m_cs);
end;

procedure TncDelay.cancel(const proc_method: TThreadMethod);
var
    i : Integer;
    item : PTncDelayRec;
begin
    EnterCriticalSection(m_cs);

    m_processing_cancelling := true;

    for i := 0 to m_runlist.Count - 1 do
    begin
        item := m_runlist[i];

        if @item.proc_method = @proc_method then
        begin
            item.cancelled := true;
        end;
    end;

    m_processing_cancelling := false;

    LeaveCriticalSection(m_cs);
end;

procedure TncDelay.cancel(const proc : TProcedure);
var
    i : Integer;
    item : PTncDelayRec;
begin
    EnterCriticalSection(m_cs);

    m_processing_cancelling := true;

    for i := 0 to m_runlist.Count - 1 do
    begin
        item := m_runlist[i];

        if @item.proc = @proc then
        begin
            item.cancelled := true;
        end;
    end;

    m_processing_cancelling := false;

    LeaveCriticalSection(m_cs);
end;

constructor TncDelay.Create();
begin
    {$IFDEF FPC}
    InitCriticalSection(m_cs);
    {$ELSE}
    InitializeCriticalSection(m_cs);
    {$ENDIF}

    m_timer := TTimer.Create(nil);
    m_timer.OnTimer := on_timer;
    m_timer.Enabled := false;
    m_timer.Interval := 100;

    m_runlist := TList.Create();
end;

destructor TncDelay.Destroy();
var
    i : Integer;
    item : PTncDelayRec;
begin
    for i := 0 to m_runlist.Count - 1 do
    begin
        item := m_runlist[i];
        Dispose(item);
    end;

    m_runlist.Free();
    m_runlist := nil;

    m_timer.Free();
    m_timer := nil;

    {$IFDEF FPC}
    DoneCriticalSection(m_cs);
    {$ELSE}
    DeleteCriticalSection(m_cs);
    {$ENDIF}

    inherited;
end;

procedure TncDelay.disable_timer;
begin
    m_timer.Enabled := false;
end;

procedure TncDelay.do_proc(const proc: TProcedure);
var
    local_proc : TProcedure;
begin
    local_proc := proc;
    TncThread.dispatch_to_main_thread(local_proc);
end;

procedure TncDelay.do_proc_method(const proc_method: TThreadMethod);
var
    local_proc_method : TThreadMethod;
begin
    local_proc_method := proc_method;
    TncThread.dispatch_to_main_thread(local_proc_method);
end;

procedure TncDelay.end_disable();
begin
    EnterCriticalSection(m_cs);

    m_disabled := false;

    LeaveCriticalSection(m_cs);
end;

procedure TncDelay.enable_timer();
begin
    m_timer.Enabled := true;
end;

procedure TncDelay.on_timer(Sender: TObject);
var
    i : Integer;
    c : Int64;
    item : PTncDelayRec;
begin
    EnterCriticalSection(m_cs);

    if (not m_processing_cancelling) and (not m_running) and (not m_disabled) then
    begin
        m_running := true;

        c := GetTickCount64();

        i := 0;
        while i < m_runlist.Count do
        begin
            item := m_runlist[i];
            if (c >= item.run_time) or item.cancelled then
            begin
                m_runlist.Delete(i);

                if item.cancelled then
                begin
                    Dispose(item);
                end
                else if Assigned(item.proc) then
                begin
                    do_proc(item.proc);
                    Dispose(item);
                end
                else if Assigned(item.proc_method) then
                begin
                    do_proc_method(item.proc_method);
                    Dispose(item);
                end
                else
                begin
                    Dispose(item);
                end;
            end
            else
            begin
                Inc(i);
            end;
        end;

        if m_runlist.Count = 0 then
        begin
            m_timer.Enabled := false;
        end;

        m_running := false;
    end;

    LeaveCriticalSection(m_cs);
end;

procedure TncDelay.run(const proc: TProcedure; const after_ms: UInt64);
var
    item : PTncDelayRec;
begin
    EnterCriticalSection(m_cs);

    if not m_disabled then
    begin
        New(item);

        item.run_time := GetTickCount64() + after_ms;
        item.proc := proc;
        item.proc_method := nil;
        item.cancelled := false;

        m_runlist.Add(item);
        TncThread.ensure_in_main_thread(enable_timer);
    end;

    LeaveCriticalSection(m_cs);
end;

procedure TncDelay.run(const proc_method: TThreadMethod; const after_ms: UInt64);
var
    item : PTncDelayRec;
begin
    EnterCriticalSection(m_cs);

    if not m_disabled then
    begin
        New(item);

        item.run_time := GetTickCount64() + after_ms;
        item.proc := nil;
        item.proc_method := proc_method;
        item.cancelled := false;

        m_runlist.Add(item);
        TncThread.ensure_in_main_thread(enable_timer);
    end;

    LeaveCriticalSection(m_cs);
end;

initialization
    init_delay();

finalization
    uninit_delay();

end.
