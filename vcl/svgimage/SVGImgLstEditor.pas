unit SVGImgLstEditor;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.ImageList,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ImgList,
  Vcl.ExtDlgs, Vcl.ExtCtrls,
  DesignIntf, DesignEditors,
  SVGImageList;

type
  TImageListEditor = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    ImagePanel: TPanel;
    Label1: TLabel;
    ImageName: TEdit;
    OkButton: TButton;
    CancelButton: TButton;
    ApplyButton: TButton;
    ListView1: TListView;
    AddButton: TButton;
    DeleteButton: TButton;
    ClearButton: TButton;
    ExportButton: TButton;
    OpenPictureDialog1: TOpenPictureDialog;
    SavePictureDialog1: TSavePictureDialog;
    ActImage: TImage;
    SVGImageList1: TSVGImageList;
    ReplaceButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ApplyButtonClick(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ExportButtonClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ImageNameExit(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListView1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ListView1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ReplaceButtonClick(Sender: TObject);
  private
    FImages: TSVGCollectionItems;
    FComponentList: TSVGImageList;
    FChanged: Boolean;
    FModified: Boolean;

    procedure BuildList(Selected: Integer);
    procedure PaintActive;
    procedure Apply;
  public
    constructor CreateImgListEditor(AOwner: TComponent;
      ASVGImgList: TSVGImageList);
    property Modified: Boolean read FModified;
  end;

  TSVGImageListProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

  TSVGImageListEditor = class(TDefaultEditor)
  protected
    procedure EditProperty(const PropertyEditor: IProperty;
      var Continue: Boolean); override;
  end;

procedure Register;

implementation

{$R *.dfm}

uses
  Winapi.GDIPAPI,
  SVG, SVGImage;

procedure Register;
begin
  RegisterComponentEditor(TSVGImageList, TSVGImageListEditor);

  RegisterPropertyEditor(TypeInfo(TSVGCollectionItems), TSVGImageList, 'Items', TSVGImageListProperty);
end;

procedure TImageListEditor.FormCreate(Sender: TObject);
begin
  ApplyButton.Enabled := False;
  ImageName.Enabled := False;
  DeleteButton.Enabled := False;
  ExportButton.Enabled := False;
  FImages := TSVGCollectionItems.Create(Self);
  FChanged := False;
  FModified := False;
end;

procedure TImageListEditor.ApplyButtonClick(Sender: TObject);
begin
  ApplyButton.Enabled := False;
  Apply;
end;

procedure TImageListEditor.ClearButtonClick(Sender: TObject);
begin
  ApplyButton.Enabled := True;
  ImageName.Enabled := False;
  ImageName.Text := '';
  ListView1.Clear;
  ActImage.Picture := nil;
  SVGImageList1.Clear;
  FChanged := True;
  FImages.Clear;
  ClearButton.Enabled := FImages.Count > 0;
end;

procedure TImageListEditor.AddButtonClick(Sender: TObject);
var
  C: Integer;
  SVG: TSVG;
  FileName: string;
  Item: TSVGCollectionItem;
begin
  if OpenPictureDialog1.Execute then
  begin
    ApplyButton.Enabled := True;
    SVG := TSVG.Create;
    for C := 0 to OpenPictureDialog1.Files.Count - 1 do
    begin
      FileName := ChangeFileExt(ExtractFileName(OpenPictureDialog1.Files[C]), '');
      try
        SVG.LoadFromFile(OpenPictureDialog1.Files[C]);
        Item := FImages.Add;
        Item.Name := FileName;
        Item.SVG := SVG;
        FChanged := True;
      finally
      end;
    end;
    SVG.Free;
    BuildList(MaxInt);
    ClearButton.Enabled := FImages.Count > 0;
  end;
end;

procedure TImageListEditor.DeleteButtonClick(Sender: TObject);
var
  C: Integer;
  Selected: Integer;
begin
  ApplyButton.Enabled := True;
  DeleteButton.Enabled := ListView1.SelCount > 0;
  ReplaceButton.Enabled := ListView1.SelCount = 1;

  Selected := ListView1.ItemIndex;
  for C := ListView1.Items.Count - 1 downto 0 do
    if ListView1.Items[C].Selected then
      FImages.Delete(C);

  FChanged := True;
  BuildList(Selected);
  ClearButton.Enabled := FImages.Count > 0;
  PaintActive;
end;

procedure TImageListEditor.ListView1DragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  Target: TListItem;
  Item: TCollectionItem;
  SIndex, DIndex: Integer;
begin
  SIndex := ListView1.ItemIndex;
  Target := ListView1.GetItemAt(X, Y);
  if Target = nil then
    Target := ListView1.GetNearestItem(Point(X, Y), sdRight);

  if Assigned(Target) then
    DIndex := ListView1.Items.IndexOf(Target)
  else
    DIndex := ListView1.Items.Count - 1;

  Item := FImages[SIndex];
  Item.Index := DIndex;
  BuildList(Item.Index);
  if SIndex <> DIndex then
  begin
    FChanged := True;
    ApplyButton.Enabled := True;
  end;
end;

procedure TImageListEditor.ListView1DragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := Source = Sender;
end;

procedure TImageListEditor.ListView1SelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  DeleteButton.Enabled := ListView1.SelCount > 0;
  ReplaceButton.Enabled := ListView1.SelCount = 1;
  PaintActive;
  ExportButton.Enabled := ListView1.SelCount = 1;
end;

procedure TImageListEditor.PaintActive;
var
  SVGGraphic: TSVGGraphic;
begin
  if ListView1.SelCount <> 1 then
  begin
    ActImage.Picture := nil;
    ImageName.Text := '';
    ImageName.Enabled := False;
    Exit;
  end;

  ActImage.Picture := nil;
  ActImage.Repaint;
  ImageName.Text := ListView1.Selected.Caption;
  ImageName.Enabled := True;
  SVGGraphic := TSVGGraphic.Create;
  SVGGraphic.AssignSVG(FImages[ListView1.ItemIndex].SVG);
  ActImage.Picture.Assign(SVGGraphic);
  SVGGraphic.Free;
end;

procedure TImageListEditor.ReplaceButtonClick(Sender: TObject);
var
  C: Integer;
  SVG: TSVG;
  FileName: string;
  Item: TSVGCollectionItem;
begin
  if OpenPictureDialog1.Execute then
  begin
    ApplyButton.Enabled := True;
    SVG := TSVG.Create;
    for C := 0 to OpenPictureDialog1.Files.Count - 1 do
    begin
      FileName := ChangeFileExt(ExtractFileName(OpenPictureDialog1.Files[C]), '');
      try
        SVG.LoadFromFile(OpenPictureDialog1.Files[C]);
        Item := FImages[ListView1.ItemIndex];
        Item.Name := FileName;
        Item.SVG := SVG;
        FChanged := True;
      finally
      end;
    end;
    SVG.Free;
    BuildList(MaxInt);
    ClearButton.Enabled := FImages.Count > 0;
  end;
end;

procedure TImageListEditor.BuildList(Selected: Integer);
var
  C: Integer;
  LI: TListItem;
begin
  ListView1.Clear;
  SVGImageList1.Clear;
  for C := 0 to FImages.Count - 1 do
  begin
    SVGImageList1.Add(FImages[C].SVG, FImages[C].Name);
    LI := ListView1.Items.Add;
    LI.ImageIndex := C;
    LI.Caption := FImages[C].Name;
  end;

  if Selected < -1 then
    Selected := -1;
  if Selected >= FImages.Count then
    Selected := FImages.Count - 1;

  ListView1.ItemIndex := Selected;
end;

procedure TImageListEditor.ExportButtonClick(Sender: TObject);
begin
  if SavePictureDialog1.Execute then
    FImages[ListView1.ItemIndex].SVG.SaveToFile(SavePictureDialog1.FileName);
end;

procedure TImageListEditor.Apply;
begin
  if not FChanged then
    Exit;
  FComponentList.Items.Assign(FImages);
  FChanged := False;
  FModified := True;
end;

constructor TImageListEditor.CreateImgListEditor(AOwner: TComponent;
  ASVGImgList: TSVGImageList);
begin
  inherited Create(AOwner);
  FComponentList := ASVGImgList;
end;

procedure TImageListEditor.OkButtonClick(Sender: TObject);
begin
  Apply;
end;

procedure TImageListEditor.FormDestroy(Sender: TObject);
begin
  FImages.Free;
end;

{ TSVGImageListPropertity }

procedure TSVGImageListProperty.Edit;
var
  Editor: TImageListEditor;
  SVGImageList: TSVGImageList;
begin
  SVGImageList := TSVGImageList(GetComponent(0));
  Editor := TImageListEditor.CreateImgListEditor(Application, SVGImageList);
  try
    Editor.ShowModal;
    if Editor.Modified then
      Modified;
  finally
    Editor.Free;
  end;
end;

function TSVGImageListProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog, paReadOnly];
end;

function TSVGImageListProperty.GetValue: string;
begin
  Result := 'SVGImages';
end;

{ TSVGImageListEditor }

procedure TSVGImageListEditor.EditProperty(const PropertyEditor: IProperty;
  var Continue: Boolean);
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'Items') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;


procedure TImageListEditor.ImageNameExit(Sender: TObject);
begin
  if FImages[ListView1.ItemIndex].Name <> ImageName.Text then
  begin
    FChanged := True;
    FImages[ListView1.ItemIndex].Name := ImageName.Text;
    ListView1.Selected.Caption := ImageName.Text;
    ApplyButton.Enabled := True;
  end;
end;

procedure TImageListEditor.FormShow(Sender: TObject);
begin
  FImages.Assign(FComponentList.Items);

  BuildList(0);
  ClearButton.Enabled := FImages.Count > 0;
end;

end.
