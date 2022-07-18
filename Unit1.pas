unit Unit1;

interface

uses
  System.SysUtils, System.IOUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, PyEnvironment,
  PyEnvironment.Embeddable, PythonEngine, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  PyEnvironment.Embeddable.Res, PyEnvironment.Embeddable.Res.Python39;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    PyCleanOnExit: Boolean;
    procedure PyEnvAfterDeactivate(Sender: TObject; const APythonVersion: string);
    procedure PyIOSendUniData(Sender: TObject; const Data: string);
  public
    { Public declarations }
    PyEng: TPythonEngine;
    PyEnv: TPyEmbeddedResEnvironment39;
    PyIO: TPythonInputOutput;
    AppHome: String;
    procedure Log(const AMsg: String);
  end;

var
  Form1: TForm1;

const
  appname: String = 'Lartis';
  pypath: String = 'python';
  pyver: String = '3.9';

implementation

{$R *.fmx}

procedure TForm1.Log(const AMsg: String);
begin
  Memo1.Lines.Add(Amsg);
end;

procedure TForm1.PyIOSendUniData(Sender: TObject; const Data: string);
begin
  Log(Data);
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  PyEnv.Deactivate;
end;

procedure TForm1.PyEnvAfterDeactivate(Sender: TObject;
  const APythonVersion: string);
begin
  if PyCleanOnExit then
    begin
      TDirectory.Delete(PyEnv.EnvironmentPath, True);
    end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  PythonCode: TStringList;
begin
  PyCleanOnExit := True;
  AppHome := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetHomePath) + appname;
  //  + System.IOUtils.TPath.DirectorySeparatorChar;
  if not DirectoryExists(AppHome) then
    ForceDirectories(AppHome);

  PyIO := TPythonInputOutput.Create(Self);
  PyIO.UnicodeIO := True;
  PyIO.OnSendUniData := PyIOSendUniData;

  PyEng := TPythonEngine.Create(Self);
  PyEng.IO := PyIO;
  PyEng.RedirectIO := True;

  PyEnv := TPyEmbeddedResEnvironment39.Create(Self);
  PyEnv.PythonEngine := PyEng;
  PyEnv.PythonVersion := pyver;

  PyEnv.AfterDeactivate := PyEnvAfterDeactivate;

  Log('Calling Setup');

  PyEnv.EnvironmentPath := AppHome + System.IOUtils.TPath.DirectorySeparatorChar + pypath;
  PyEng.DllPath := PyEnv.EnvironmentPath;
  PyEnv.Setup(pyver);

  Log('Env Path = ' + PyEnv.EnvironmentPath);
  Log('Eng Lib = ' + PyEng.DllName);
  Log('Eng Libpath = ' + PyEng.DllPath);
  Log('Calling Activate');

  PyEnv.Activate(pyver);
  PythonCode := TStringList.Create;
  PythonCode.Add('print("import sys")');
  PythonCode.Add('print("Hello World ", sys.version)');
  PyEng.ExecStrings(PythonCode);

  Log('Done');
end;

end.
