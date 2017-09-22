unit SVGImageRegister;

interface

procedure Register;

implementation

uses
  System.Classes,
  SVGImage, SVGImageList, SVGSpeedButton;

procedure Register;
begin
  RegisterComponents('MWK', [TSVGImage, TSVGImageList, TSVGSpeedButton]);
end;


end.
