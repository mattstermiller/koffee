using System;
using System.Globalization;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Markup;

namespace KoffeeUI {
    /// <summary>
    /// Returns the width of the TextBlock if it is currently not wrapped, otherwise a larger width
    /// </summary>
    public class UnwrappedTextBlockWidthConverter : MarkupExtension, IMultiValueConverter {
        public object Convert(object[] values, Type targetType, object parameter, CultureInfo culture) {
            var textBlock = (TextBlock)values[0];
            return GetUnwrappedTextBlockWidth(textBlock);
        }

        private static double GetUnwrappedTextBlockWidth(TextBlock sourceTextBlock) {
            var textBlock = new TextBlock {
                Text = sourceTextBlock.Text,
                FontSize = sourceTextBlock.FontSize,
                TextWrapping = TextWrapping.NoWrap
            };
            textBlock.Measure(new Size(double.PositiveInfinity, double.PositiveInfinity));
            textBlock.Arrange(new Rect(textBlock.DesiredSize));
            var margin = sourceTextBlock.Margin;
            return textBlock.ActualWidth + margin.Left + margin.Right;
        }

        public object[] ConvertBack(object value, Type[] targetTypes, object parameter, CultureInfo culture) =>
            throw new NotImplementedException();

        public override object ProvideValue(IServiceProvider serviceProvider) => this;
    }
}