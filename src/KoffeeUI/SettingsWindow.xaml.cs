using System.Diagnostics;
using System.Windows;
using System.Windows.Documents;

namespace KoffeeUI {
    public partial class SettingsWindow : Window {
        public SettingsWindow() {
            InitializeComponent();
            RegisterHyperlinkClick(ReadmeLink);
        }

        private void RegisterHyperlinkClick(params Hyperlink[] controls) {
            foreach (var control in controls)
                control.RequestNavigate += (s, e) =>
                    Process.Start(e.Uri.ToString());
        }
    }
}
