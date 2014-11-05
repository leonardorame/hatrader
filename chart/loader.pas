unit loader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Symbols,
  //fphttpclient
  httpsend;

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
    FData: TStringList;
    //FHttpClient: TFPHTTPClient;
    procedure OnData;
    procedure WriteFeedBack;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
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
    FData: TStringList;
    //FHttpClient: TFPHTTPClient;
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
    FOnGetData(FData.Text);
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
  FUrl := AUrl;
  //FHttpClient := TFPHTTPClient.Create(nil);
  FData := TStringList.Create;
  FreeOnTerminate := True;
end;

destructor TGetAllDataThread.Destroy;
begin
  //FHttpClient.Free;
  FData.Free;
  gThreadList.Remove(Self);
  inherited Destroy;
end;

procedure TGetAllDataThread.Execute;
begin
  FFeedBackStr:= 'Getting all symbols';
  Synchronize(@WriteFeedBack);
  try
    try
      if HttpGetText(FUrl, FData) then
      //FHttpClient.Get(FUrl, FData);
      //if FHttpClient.ResponseStatusCode = 200 then
      begin
        FFeedBackStr := 'All symbols data loading done.';
        Synchronize(@WriteFeedBack);
        Synchronize(@OnData);
      end
      else
      begin
        //FFeedBackStr:= 'Error loading all symbols data (' + FHttpClient.ResponseStatusText + ')';
        FFeedBackStr:= 'Error loading all symbols data.';
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
  FSymbol.PrepareArray(FData.Text);
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
  //FHttpClient := TFPHTTPClient.Create(nil);
  FData := TStringList.Create;
end;

destructor TGetDataThread.Destroy;
begin
  FData.Free;
  //FHttpClient.Free;
  gThreadList.Remove(Self);
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

      //FHttpClient.Get(lUrl, FData);
      //if FHttpClient.ResponseStatusCode = 200 then
      if HttpGetText(lUrl, FData) then
      begin
        FFeedBackStr := FSymbol.Name + ' loading done.';
        Synchronize(@WriteFeedBack);
        Synchronize(@OnData);
      end
      else
      begin
        //FFeedBackStr := 'Error loading ' + FSymbol.Name + ' (' + FHttpClient.ResponseStatusText + ')';
        FFeedBackStr := 'Error loading ' + FSymbol.Name;
        Synchronize(@WriteFeedBack);
        //raise Exception.Create('Error loading ' + FSymbol.Name + ' (' + FHttpClient.ResponseStatusText + ')');
      end;
    except
      on E: Exception do
      begin
        FFeedBackStr:= E.Message; // 'Exception in TGetDataThread.Execute';
        Synchronize(@WriteFeedBack);
      end;
    end;
  finally
    //Terminate;
  end;
end;


end.

