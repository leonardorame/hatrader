unit loader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Symbols,
  fphttpclient;

type
  TOnFeedBack = procedure (AFeedBack: string) of object;
  TOnData = procedure (AData: string) of object;

  { TGetDataThread }

  TGetDataThread = class(TThread)
  private
    FSymbol: TSymbol;
    FOnFeedBack: TOnFeedBack;
    FOnGetData: TNotifyEvent;
    FFeedBackStr: string;
    FData: string;
    FHttpClient: TFPHTTPClient;
    procedure OnData;
    procedure WriteFeedBack;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
    property Data: string read FData;
    property Symbol: TSymbol read FSymbol write FSymbol;
    property OnGetData: TNotifyEvent read FOnGetData write FOnGetData;
    property OnFeedBack: TOnFeedBack read FOnFeedBack write FOnFeedBack;
  end;

  { TGetAllDataThread }

  TGetAllDataThread = class(TThread)
  private
    FOnFeedBack: TOnFeedBack;
    FOnGetData: TOnData;
    FFeedBackStr: string;
    FData: string;
    FHttpClient: TFPHTTPClient;
    FUrl: string;
    procedure OnData;
    procedure WriteFeedBack;
  public
    constructor Create(AUrl: string);
    destructor Destroy; override;
    procedure Execute; override;
    property OnGetData: TOnData read FOnGetData write FOnGetData;
    property OnFeedBack: TOnFeedBack read FOnFeedBack write FOnFeedBack;
  end;

var
  gThreadList: TThreadList;

implementation

{ TGetAllDataThread }

procedure TGetAllDataThread.OnData;
begin
  if Assigned(FOnGetData) then
    FOnGetData(FData);
end;

procedure TGetAllDataThread.WriteFeedBack;
begin
  if Assigned(FOnFeedBack) then
    FOnFeedBack(FFeedBackStr);
end;

constructor TGetAllDataThread.Create(AUrl: string);
begin
  inherited Create(True);
  gThreadList.Add(Self);
  Priority:= tpLower;
  FreeOnTerminate := True;
  FUrl := AUrl;
  FHttpClient := TFPHTTPClient.Create(nil);
end;

destructor TGetAllDataThread.Destroy;
begin
  gThreadList.Remove(Self);
  FHttpClient.Free;
  inherited Destroy;
end;

procedure TGetAllDataThread.Execute;
begin
  FFeedBackStr:= 'Getting all symbols';
  Synchronize(@WriteFeedBack);
  try
    try
      FData := FHttpClient.Get(FUrl);
      if FHttpClient.ResponseStatusCode = 200 then
      begin
        FFeedBackStr := 'All symbols data loading done.';
        Synchronize(@WriteFeedBack);
        Synchronize(@OnData);
      end
      else
      begin
        FFeedBackStr:= 'Error loading all symbols data (' + FHttpClient.ResponseStatusText + ')';
        Synchronize(@WriteFeedBack);
        //raise Exception.Create('Error loading all symbols data (' + FHttpClient.ResponseStatusText + ')');
      end;
    except
      on E: Exception do
      begin
        FFeedBackStr:= E.Message;
        Synchronize(@WriteFeedBack);
      end;
    end;
  finally
    Terminate;
  end;
end;

{ TGetDataThread }

procedure TGetDataThread.OnData;
begin
  // cargamos los datos dentro
  // de este Synchronize
  FSymbol.PrepareArray(FData);
end;

procedure TGetDataThread.WriteFeedBack;
begin
  if Assigned(FOnFeedBack) then
    FOnFeedBack(FFeedBackStr);
end;

constructor TGetDataThread.Create;
begin
  inherited Create(True);
  gThreadList.Add(Self);
  Priority:= tpLower;
  FreeOnTerminate := True;
  FHttpClient := TFPHTTPClient.Create(nil);
end;

destructor TGetDataThread.Destroy;
begin
  gThreadList.Remove(Self);
  FHttpClient.Free;
  inherited Destroy;
end;

procedure TGetDataThread.Execute;
var
  lUrl: string;

begin
  FFeedBackStr:= 'Getting ' + FSymbol.Name;
  Synchronize(@WriteFeedBack);
  try
    try
      lUrl := FSymbol.FilePath;
      FData := FHttpClient.Get(lUrl);
      if FHttpClient.ResponseStatusCode = 200 then
      begin
        FFeedBackStr := FSymbol.Name + ' loading done.';
        Synchronize(@WriteFeedBack);
        Synchronize(@OnData);
      end
      else
      begin
        FFeedBackStr := 'Error loading ' + FSymbol.Name + ' (' + FHttpClient.ResponseStatusText + ')';
        Synchronize(@WriteFeedBack);
        //raise Exception.Create('Error loading ' + FSymbol.Name + ' (' + FHttpClient.ResponseStatusText + ')');
      end;
    except
      on E: Exception do
      begin
        FFeedBackStr:= 'Exception in TGetDataThread.Execute';
        Synchronize(@WriteFeedBack);
      end;
    end;
  finally
    Terminate;
  end;
end;


end.

