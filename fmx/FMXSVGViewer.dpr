program FMXSVGViewer;

uses
  System.StartUpCopy,
  FMX.Forms,
  FmxSVGViewerUnit in 'FmxSVGViewerUnit.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
