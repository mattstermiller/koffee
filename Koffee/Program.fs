module Program

let assemblyName = System.Reflection.Assembly.GetExecutingAssembly().GetName().Name

let getXamlResource xaml = 
  sprintf "/%s;component/%s" assemblyName xaml
  |> fun nme -> System.Uri(nme, System.UriKind.Relative)
  |> System.Windows.Application.LoadComponent
  :?> _

let mainWindow : System.Windows.Window = getXamlResource "MainWindow.xaml"

[<System.STAThread>]
do System.Windows.Application().Run(mainWindow) |> ignore
